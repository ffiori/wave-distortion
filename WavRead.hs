module WavRead (readWav) where

import WavTypes
import Utilities (getInt, getString, recursiveGet, safelyRemoveFile, makeTemps)

import qualified Data.ByteString as BS
import Data.Int
import System.IO
import System.IO.Temp (openBinaryTempFile)
import System.Directory (removeFile, doesFileExist)
import Data.Conduit
import Control.Monad.IO.Class (liftIO)

import qualified Control.Exception as E
import System.IO.Error


readWav :: FilePath -> IO WavFile
readWav path = do
  handle <- (openBinaryFile path ReadMode) `catchIOError` catcher
  parseHeader handle
    where
      catcher :: IOError -> IO Handle
      catcher e = 
        let msg = if isDoesNotExistError e then ". El archivo no existe."
                  else if isAlreadyInUseError e then ". El archivo está siendo usado."
                  else "."
        in do putStrLn ("No se pudo abrir el archivo "++(show path)++msg++"\nError: "++(show e))
              E.throw e


-- Headers ---------------------------------------------------------------------

parseHeader :: Handle -> IO WavFile
parseHeader h = do
   rh <- parsehRIFF h `catchIOError` catcher "RIFF"
   fh <- parsehFMT h `catchIOError` catcher "format"
   dh <- parsehDATA h rh fh `catchIOError` catcher "data"
   return W { riffheader = rh
            , fmtheader = fh
            , dataheader = dh
            }
  where
    catcher :: String -> IOError -> IO a
    catcher s e = putStrLn ("Error en header "++s++".") >> E.throw e

parsehRIFF :: Handle -> IO HRIFF
parsehRIFF h = do fields <- recursiveGet h riffS
                  return HR { chunkID   = getString (fields!!0)
                            , chunkSize = getInt (fields!!1)
                            , format    = getString (fields!!2)
                            }

parsehFMT :: Handle -> IO Hfmt
parsehFMT h = do
  dfields <- recursiveGet h defaultS
  let id = getString (dfields!!0)
      sz = getInt (dfields!!1)
  if id/="fmt " 
    then do
      hSeek h RelativeSeek (fromIntegral sz)
      putStrLn $ "    chunk descartado de ID:"++id++"."
      parsehFMT h --descarto todos los chunks opcionales del formato WAVE.
    else do 
      fields <- recursiveGet h fmtS
      let format = getInt (fields!!0)
      fieldsExt <- if format == wave_format_extended
                     then recursiveGet h fmtExtS
                     else if fromIntegral sz > sum fmtS -- algunos formatos llevan este campo en 0 aunque no sea el extendido.
                            then recursiveGetn h fmtExtS (fromIntegral sz - (sum fmtS))
                            else return []
      return HF { subchunk1ID   = id
                , subchunk1Size = sz
                , audioFormat   = format
                , numChannels   = getInt (fields!!1)
                , sampleRate    = getInt (fields!!2)
                , byteRate      = getInt (fields!!3)
                , blockAlign    = getInt (fields!!4)
                , bitsPerSample = getInt (fields!!5)
                , cbSize =
                    if length fieldsExt > 0
                      then Just $ getInt $ fieldsExt!!0
                      else Nothing
                , validBitsPerSample =
                    if length fieldsExt > 1
                      then Just $ getInt $ fieldsExt!!1
                      else Nothing
                , chMask = 
                    if length fieldsExt > 2
                      then Just $ getInt $ fieldsExt!!2
                      else Nothing
                , subFormat = 
                    if length fieldsExt > 3
                      then Just $ getInt $ fieldsExt!!3
                      else Nothing
                , check =
                    if length fieldsExt > 4
                      then Just $ getString $ fieldsExt!!4
                      else Nothing
                }
  where
    recursiveGetn :: Handle -> [Int] -> Int -> IO [BS.ByteString]
    recursiveGetn _ _ 0 = return []
    recursiveGetn _ [] n = error $ "Faltan "++(show n)++" bytes en el header format."
    recursiveGetn h szs n =
      let next = head szs
      in if n < next
           then error $ "Faltan "++(show $ next-n)++" bytes en el header format."
           else do b <- BS.hGet h next
                   bs <- recursiveGetn h (tail szs) (n-next)
                   return $ b:bs
      
    

parsehDATA :: Handle -> HRIFF -> Hfmt -> IO Hdata
parsehDATA h rh fh = 
  let format = audioFormat fh
      format2 = case subFormat fh of
                   Just x -> x
                   Nothing -> 1
  in
    if format > 1 || (format == wave_format_extended && format2 > 1)
      then error $ "Archivo comprimido o de punto flotante no soportado por el programa. Formato de compresión "++(show format)++ if format /= wave_format_extended then "." else ". Subformato "++(show format2)++"."
      else
        if bitsPerSample fh > 32 || mod (bitsPerSample fh) 8 /= 0
          then error $ "Profundidad de muestras no soportada por el programa. Sólo se admiten muestras de 8, 16, 24 y 32 bits. BitsPerSample "++(show $ bitsPerSample fh)
          else do
            fields <- recursiveGet h defaultS
            let id = getString (fields!!0)
                sz = getInt (fields!!1)
            if id /= "data"
              then do
                hSeek h RelativeSeek (fromIntegral sz)
                putStrLn $ "    chunk descartado de ID:"++id++"."
                parsehDATA h rh fh --descarto todos los chunks opcionales del formato WAVE.
              else do
                chFilePaths <- parseData sz fh h  --escribe los canales en archivos temporales separados (uno por canal)
                return HD { chunk2ID   = id 
                          , chunk2Size = sz
                          , chFiles = chFilePaths
                          }


-- Data ------------------------------------------------------------------------

parseData :: Int32 -> Hfmt -> Handle -> IO [FilePath]
parseData sz fh h =
  let sampsz = div (bitsPerSample fh) 8
      nc = numChannels fh
      nblocks = div sz (sampsz*nc)
      wf = W { fmtheader = fh, dataheader=undefined, riffheader=undefined }
  in E.bracketOnError
     (makeTemps wf) --armo los archivos de canales temporales.
     (\ chFiles -> sequence $ map (hClose.snd>>safelyRemoveFile.fst) chFiles) --si algo falla mientras los estoy llenando los borro.
     (\ chFiles -> do getSamples nc sampsz h $$ parsePerCh nblocks chFiles
                      return $ map fst chFiles )

--SOURCE
--parsea una muestra para cada canal. Hay n canales, muestras de tamaño sampsz.
getSamples :: Int32 -> Int32 -> Handle -> Source IO [BS.ByteString]
getSamples n sampsz hsource = do
  eof <- liftIO $ hIsEOF hsource
  if eof
    then error $ "Posible error en el campo tamaño del header data. Faltan obtener "++(show n)++" samples! Como mínimo..."
    else do
      samples <- liftIO $ sequence [ BS.hGet hsource (fromIntegral sampsz) | j<-[1..n] ] --sequence :: Monad m => [m a] -> m [a]
      yield samples
      getSamples n sampsz hsource

--SINK
--escribe una muestra en cada canal (o sea un bloque de muestras), nblocks veces.
parsePerCh :: Int32 -> [(FilePath,Handle)] -> Sink [BS.ByteString] IO ()
parsePerCh 0       chFiles = do
  liftIO $ sequence $ map (hFlush.snd>>hClose.snd) chFiles --flusheo y cierro los handles.
  return ()
parsePerCh nblocks chFiles = do
  x <- await
  case x of
    Nothing -> error "No hay más samples!"
    Just samples -> 
      if null chFiles
        then error "No hay canales!"
        else do
          liftIO $ sequence $ map (\((_,h),smpl) -> BS.hPut h smpl) (zip chFiles samples)
          parsePerCh (nblocks-1) chFiles
