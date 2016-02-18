module Utilities
    ( fromSampletoByteString
    , fromByteStringtoSample
    , recursiveGet
    , getInt
    , getString
    , makeTemps
    , safelyRemoveFile
    ) where

import WavTypes

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Char (chr)
import Data.Bits
import Data.Word

import System.IO
import System.IO.Temp (openBinaryTempFile)
import System.Directory (removeFile, doesFileExist)

import qualified Control.Exception as E
import System.IO.Error


fromSampletoByteString :: Int32 -> Sample -> BS.ByteString
fromSampletoByteString sz' x =
  let sz = fromIntegral sz'
      loop :: Int -> Sample -> [Word8] -> [Word8]
      loop 0 x ws = ws
      loop n x ws = loop (n-1) (shiftR x 8) (((fromIntegral x)::Word8):ws)
      maxlim = shiftL 1 (sz*8-1) - 1
      minlim = -maxlim - 1
      value = if x>maxlim 
                then maxlim  --si el sample tiene un valor por fuera de los límites de la representación, se trunca.
                else if x<minlim then minlim else x
  in BS.pack $ reverse $ if sz==1 
                           then loop sz (value+128) [] --si es 8 bits se almacena como unsigned!
                           else loop sz value []


fromByteStringtoSample :: Int32 -> BS.ByteString -> Sample
fromByteStringtoSample sz x = 
  if BS.length x < (fromIntegral sz)
    then error $ "Se está intentando parsear una muestra de "++(show sz)++" bytes pero en cambio sólo hay "++(show $ BS.length x)++" disponibles."
    else
      let xs' = reverse $ BS.unpack x --reverse porque es little endian!
          loop :: [Word8] -> Int32 -> Sample
          loop [] n = fromIntegral $ shiftR (shiftL n gap) gap --shifteo para mantener el signo
          loop (x:xs) n = loop xs $ (shiftL n 8) .|. ((fromIntegral x) .&. 255) --hago & 255 para que me deje solamente los últimos 8 bits (si x es negativo me rellena con unos los primeros 24 bits)
          gap = (4-(fromIntegral sz))*8 --bits que sobran a la izquierda del número. Ojo: Se asume Int32 de 32 bits.
      in if sz==1 then (fromIntegral (head xs') - 128) else loop xs' 0 --si es 8 bits se almacena como unsigned!


--devuelve los campos que se quieren parsear con tamaños en hSizes en una lista de LazyByteStrings.
recursiveGet :: Handle -> [Int] -> IO [BS.ByteString]
recursiveGet h hSizes = sequence $ map (BS.hGet h) hSizes


--parsea un signed Int de 32bits en little-endian.
getInt :: BS.ByteString -> Int32
getInt le = fromByteStringtoSample (fromIntegral $ BS.length le) le


-- convierte una ByteString a String.
getString :: BS.ByteString -> String
getString bs = map (chr.fromIntegral) (BS.unpack bs)


-- genera nuevos temporales para tantos canales como tenga wf.
makeTemps :: WavFile -> IO [(FilePath,Handle)]
makeTemps wf =
  let nc = numChannels $ fmtheader wf
      makeTemps' :: Int32 -> IO [(FilePath,Handle)]
      makeTemps' 0 = return []
      makeTemps' n = E.bracketOnError
        (openBinaryTempFile "." ("ch"++(show (nc-n))++"_.tmp")) 
        (\ (path,_) -> do safelyRemoveFile path
                          sequence $ map safelyRemoveFile (chFiles $ dataheader wf) )
        (\ tmp -> do tmps <- makeTemps' (n-1)
                     return $ tmp:tmps )
  in makeTemps' nc

--borra un archivo si es que existe.
safelyRemoveFile :: FilePath -> IO ()
safelyRemoveFile path = removeFile path `catchIOError` catcher
  where catcher :: IOError -> IO ()
        catcher e = if isDoesNotExistError e then return () else E.throw e



-- Deprecated zone -------------------------------------------------------------

{-
getInt :: BS.ByteString -> Int32
getInt le =
  case BS.length le of
    1 -> fromIntegral (runGet getWord8 (BL.fromStrict le)) - 128                    --el estándar dice que muestras de 8bits son unsigned. De todos modos las paso a signed con el -128.
    2 -> fromIntegral (fromIntegral (runGet getWord16le (BL.fromStrict le))::Int16) --primero parseo como Int16 y después como Int para preservar el signo.
    3 -> fromIntegral $ runGet getWord24le (BL.fromStrict le)                       --getWord24le devuelve signed.
    4 -> fromIntegral $ runGet getWord32le (BL.fromStrict le)
    _ -> error $ "getInt: longitud mayor a 4 bytes o 0 bytes. " ++ (show le)

--función no definida en la familia getWordnle.
getWord24le :: Get Word32
getWord24le = do
  x <- getWord8
  y <- getWord8
  z <- getWord8
  let z' = shiftR (shiftL ((fromIntegral z)::Int32) 24) 8 --lo corro 24 y vuelvo 8 para mantener el signo (en vez de correrlo 16 de una)
      y' = shiftL ((fromIntegral y)::Int32) 8
  return $ fromIntegral (z' .|. y') .|. (fromIntegral x)
-}

{-
-- versión no segura
makeTemps :: WavFile -> IO [(FilePath,Handle)]
makeTemps wf = let nc = numChannels $ fmtheader wf
               in sequence [ openBinaryTempFile "." ("ch"++(show i)++"_.tmp") | i<-[0..nc-1] ]
-}
