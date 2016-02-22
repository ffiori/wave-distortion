{-
    En el programa se usará la versión canónica del formato WAVE,
    es decir, que sólo se divide en tres partes (RIFF, fmt, data).
-}


module WavTypes where

import Data.Int

data WavFile = W { riffheader :: HRIFF
                 , fmtheader :: Hfmt
                 , dataheader :: Hdata
                 }

type Sample = Int32


-- Headers --

-- Aunque no todos los campos sean de 32 bits los represento a todos los que
-- sean valores numéricos con Int32 para simplicidad de código.

data HRIFF = HR { chunkID :: String   --big endian
                , chunkSize :: Int32  --tamaño en bytes del archivo a partir de acá (o sea sin incluir los 4 bytes de chunkID ni los 4 bytes de chunkSize)
                , format :: String    --big endian
                }

data Hfmt = HF { subchunk1ID :: String  --big endian
               , subchunk1Size :: Int32
               , audioFormat :: Int32
               , numChannels :: Int32
               , sampleRate :: Int32
               , byteRate :: Int32
               , blockAlign :: Int32 
               , bitsPerSample :: Int32
               , cbSize :: Maybe Int32
               , validBitsPerSample :: Maybe Int32
               , chMask :: Maybe Int32
               , subFormat :: Maybe Int32
               , check :: Maybe String
               }

data Hdata = HD { chunk2ID :: String    --big endian
                , chunk2Size :: Int32
                , chFiles :: [FilePath]
                }


-- Constantes --

headerSz :: Int
headerSz = riffSz + fmtSz + hdataSz

headerExtSz :: Int
headerExtSz = headerSz + (sum fmtExtS)

riffSz :: Int
riffSz = sum riffS

fmtSz :: Int
fmtSz = sum (defaultS++fmtS)

hdataSz :: Int
hdataSz = sum defaultS

riffS :: [Int]    --tamaños de los campos del header RIFF
riffS = [4,4,4]

fmtS :: [Int]     --tamaños de los campos del header fmt, sin tener en cuenta los primeros 8 bytes.
fmtS = [2,2,4,4,2,2]

fmtExtS :: [Int]  --tamaños de los campos de la extensión del header fmt, sin tener en cuenta los primeros 8 bytes.
fmtExtS = [2,2,4,2,14]

defaultS :: [Int] --tamaños de los campos chunkID, chunkSize que los tienen que tener todos los chunks.
defaultS = [4,4]

wave_format_extended :: Int32
wave_format_extended = -2
