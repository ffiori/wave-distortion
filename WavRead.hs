--HACER CONTROL DE ERRORES USANDO try CON merror. IDEM EN WAVWRITE

module WavRead (readWav) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Char
import Data.Bits
import Data.Binary.Get
import Data.Word

import System.IO
import System.IO.Temp (openBinaryTempFile)
import Data.Conduit
import Control.Monad.IO.Class (liftIO)

import WavTypes


class Monad m => Lector m where
    merror :: String -> m a
    getWord8' :: m Word8
    getWord16le' :: m Word16
    getWord24le' :: m Word32
    getWord32le' :: m Word32
    getLazyByteString' :: Int -> m BS.ByteString
    runGet' :: m a -> BS.ByteString -> a
    
instance Lector Get where
    merror = error
    getLazyByteString' = getByteString
    getWord8' = getWord8
    getWord16le' = getWord16le
    getWord24le' = getWord24le
    getWord32le' = getWord32le
    runGet' ma bs = runGet ma (BL.fromStrict bs)

type MonadaLectora = Get


readWav :: FilePath -> IO WavFile
readWav path = do handle <- openBinaryFile path ReadMode
                  parseHeader handle


-- Headers --

parseHeader :: Handle -> IO WavFile
parseHeader h = do rh <- parsehRIFF h
                   fh <- parsehFMT h
                   dh <- parsehDATA h rh fh
                   return W { riffheader = rh
                            , fmtheader = fh
                            , dataheader = dh
                            }

parsehRIFF :: Handle -> IO HRIFF
parsehRIFF h = do fields <- recursiveGet h riffS
                  return HR { chunkID   = getString (fields!!0)
                            , chunkSize = getInt (fields!!1)
                            , format    = getString (fields!!2)
                            }

parsehFMT :: Handle -> IO Hfmt
parsehFMT h = do fields <- recursiveGet h fmtS
                 let id = getString (fields!!0)
                     sz = getInt (fields!!1)
                 if id/="fmt " then do liftIO $ hSeek h RelativeSeek (fromIntegral sz)
                                       liftIO $ putStrLn $ "    chunk descartado de ID:"++id++"."
                                       parsehFMT h --descarto todos los chunks opcionales del formato WAVE.
                 else return HF { subchunk1ID   = id
                                , subchunk1Size = sz
                                , audioFormat   = getInt (fields!!2)
                                , numChannels   = getInt (fields!!3)
                                , sampleRate    = getInt (fields!!4)
                                , byteRate      = getInt (fields!!5)
                                , blockAlign    = getInt (fields!!6)
                                , bitsPerSample = getInt (fields!!7)
                                }

parsehDATA :: Handle -> HRIFF -> Hfmt -> IO Hdata
parsehDATA h rh fh = 
    if audioFormat fh /= 1
    then error $ "Archivo comprimido no soportado por el programa. Formato de compresión "++(show $ audioFormat fh)
    else if bitsPerSample fh > 32 || mod (bitsPerSample fh) 8 /= 0
    then error $ "Profundidad de muestras no soportada por el programa. Sólo se admiten muestras de 8, 16, 24 y 32 bits. BitsPerSample "++(show $ bitsPerSample fh)
    else do
        fields <- recursiveGet h dataS
        let id = getString (fields!!0)
            sz = getInt (fields!!1)
        if id/="data" then do liftIO $ hSeek h RelativeSeek (fromIntegral sz)
                              liftIO $ putStrLn $ "    chunk descartado de ID:"++id++"."
                              parsehDATA h rh fh --descarto todos los chunks opcionales del formato WAVE.
                      else do
                          chFilePaths <- parseData sz fh h  --escribe los canales en archivos temporales separados (uno por canal)
                          liftIO $ putStrLn id
                          return HD { chunk2ID   = id 
                                    , chunk2Size = sz
                                    , dat        = [Channel {chID=0, chData=[]}]
                                    , chFiles = chFilePaths
                                    }


-- Data --

parseData :: Int -> Hfmt -> Handle -> IO [FilePath]
parseData sz fh h = let sampsz = div (bitsPerSample fh) 8
                        nc = numChannels fh
                        nblocks = div sz (sampsz*nc)
                        makeTemps :: Int -> IO [(FilePath,Handle)]
                        makeTemps 0 = return []
                        makeTemps n = do tmps <- makeTemps (n-1)
                                         tmp <- openBinaryTempFile "." ("ch"++(show (nc-n))++"_.tmp")
                                         return $ tmp : tmps
                    in do chFiles <- makeTemps nc --armo los archivos de canales temporales.
                          getSamples nc sampsz h $$ parsePerCh nblocks chFiles
                          return $ map fst chFiles

--SOURCE
--parsea una muestra para cada canal. Hay n canales, muestras de tamaño sampsz.
getSamples :: Int -> Int -> Handle -> Source IO [BS.ByteString]
getSamples n sampsz hsource = do
        eof <- liftIO $ hIsEOF hsource
        if eof
            then error $ "Falta obtener "++(show n)++" samples! Como mínimo..."
            else do
                samples <- liftIO $ sequence [ BS.hGet hsource sampsz | j<-[1..n] ] --sequence :: Monad m => [m a] -> m [a]
                yield samples
                getSamples n sampsz hsource

--SINK
--escribe una muestra en cada canal (o sea un bloque de muestras), nblocks veces.
parsePerCh :: Int -> [(FilePath,Handle)] -> Sink [BS.ByteString] IO ()
parsePerCh 0       chFiles = do liftIO $ sequence $ map (hFlush.snd>>hClose.snd) chFiles --flusheo y cierro los handles.
                                return ()
parsePerCh nblocks chFiles = do x <- await
                                case x of
                                     Nothing -> error "No hay más samples!"
                                     Just samples -> 
                                         if length chFiles == 0 
                                         then error "No hay canales!"
                                         else do
                                             liftIO $ sequence $ map (\((_,h),smpl) -> BS.hPut h smpl) (zip chFiles samples)
                                             parsePerCh (nblocks-1) chFiles


-- Utilidades --

--devuelve los campos que se quieren parsear con tamaños en hS en una lista de LazyByteStrings.
recursiveGet :: Handle -> [Int] -> IO [BS.ByteString]
recursiveGet h [] = return []
recursiveGet h hS = do field <- BS.hGet h (head hS)
                       fs <- recursiveGet h (tail hS)
                       return (field:fs)

--parsea un signed Int de 32bits en little-endian.
getInt :: BS.ByteString -> Int
getInt le = case BS.length le of
                1 -> fromIntegral (runGet' (getWord8'::MonadaLectora Word8) le) - 128                     --el estándar dice que muestras de 8bits son unsigned. De todos modos las paso a signed con el -128.
                2 -> fromIntegral (fromIntegral (runGet' (getWord16le'::MonadaLectora Word16) le)::Int16) --primero parseo como Int16 y después como Int para preservar el signo.
                3 -> fromIntegral $ runGet' (getWord24le'::MonadaLectora Word32) le                       --getWord24le devuelve signed.
                4 -> fromIntegral $ runGet' (getWord32le'::MonadaLectora Word32) le
                _ -> error $ "getInt: longitud mayor a 4 bytes o 0 bytes. " ++ (show le)
                
getString :: BS.ByteString -> String
getString bs = map (chr.fromIntegral) (BS.unpack bs)

--función no definida en la familia getWordnle.
getWord24le :: Lector m => m Word32
getWord24le = do x <- getWord8'
                 y <- getWord8'
                 z <- getWord8'
                 let z' = shiftR (shiftL ((fromIntegral z)::Int32) 24) 8 --lo corro 24 y vuelvo 8 para mantener el signo (en vez de correrlo 16 de una)
                     y' = shiftL ((fromIntegral y)::Int32) 8
                 return $ fromIntegral (z' .|. y') .|. (fromIntegral x)
