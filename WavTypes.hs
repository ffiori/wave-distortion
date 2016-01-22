-- Formato WAV general
-- https://ccrma.stanford.edu/courses/422/projects/WaveFormat/

-- Explicados algunos detalles
-- http://web.archive.org/web/19991115123323/http://www.borg.com/~jglatt/tech/wave.htm

{-
    En el programa se usará la versión canónica del formato WAVE,
    es decir, que sólo se divide en tres partes (RIFF, fmt, data).
-}
{-# LANGUAGE BangPatterns #-}

module WavTypes where

import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Vector as V

import qualified Data.Foldable as Foldable
import Control.Parallel.Strategies
--import Control.Parallel

data WavFile = W { riffheader :: HRIFF
                 , fmtheader :: Hfmt
                 , dataheader :: Hdata
                 }


-- Headers --

data HRIFF = HR { chunkID :: String --big endian
                , chunkSize :: Int
                , format :: String  --big endian
                }

data Hfmt = HF { subchunk1ID :: String  --big endian
               , subchunk1Size :: Int
               , audioFormat :: Int
               , numChannels :: Int
               , sampleRate :: Int
               , byteRate :: Int
               , blockAlign :: Int 
               , bitsPerSample :: Int
               }

data Hdata = HD { chunk2ID :: String    --big endian
                , chunk2Size :: Int
                , dat :: AudioData --TODO acá puede ir el path al archivo temporal donde voy guardando los canales de audio, o puedo hacer un archivo por canal.
                , chFiles :: [FilePath]
                }


-- Data --

class WaveContainer c where
    cmap :: (t -> u) -> c t -> c u
    cfoldl :: (t -> u -> t) -> t -> c u -> t
    cempty :: c t
    cappend :: c t -> c t -> c t
    clength :: c t -> Int
    ccons :: t -> c t -> c t --tremolo
    chead :: c t -> t   --tremolo
    ctail :: c t -> c t --tremolo
    cisEmpty :: c t -> Bool --tremolo
    creplicate :: Int -> a -> c a --delay
    czipWith ::  (t -> u -> v) -> c t -> c u -> c v --delay

    
type Container = []
type Sample = Int

data Channel = Channel { chID :: Int                --identificador del canal
                       , chData :: Container Sample --datos del canal. WaveContainer Container se verifica en las funciones que use luego.
                       }

type AudioData = [Channel] --no parece mala elección el tipo [] ya que la cantidad de canales no es muy grande.


-- Instancias --
 
instance WaveContainer [] where
--    cmap = (parMap rseq) --genera muchísimas sparks
    cmap = map
    -- tardan lo mismo:
    --cmap f l = let fcons !s !e = ccons (f e) s
    --           in reverse $ Foldable.foldl' fcons cempty l
    cfoldl = L.foldl'   --para que no sea tan lazy y me reviente el stack. http://book.realworldhaskell.org/read/profiling-and-optimization.html#id678431
    cempty = []
    cappend = (++)      --O(m+n)
    clength = length    --O(n)
    ccons = (:)         --O(1)
    chead = head
    ctail = tail        --O(1)
    cisEmpty x = case x of 
                    [] -> True
                    _  -> False
    creplicate = replicate
    czipWith = zipWith

                    
instance WaveContainer S.Seq where
    cmap f = S.mapWithIndex (\_ -> f)
    cfoldl = Foldable.foldl'
    cempty = S.empty
    cappend = (S.><)    --O(lg(min(m,n)))
    clength = S.length  --O(1)
    ccons = (S.<|)      --O(1)
    chead s = case S.viewl s of --O(1)
                S.EmptyL  -> error "EmptyL"
                a S.:< ss -> a
    ctail = S.drop 1    --O(lg(min(1,n-1))) = O(1)
    cisEmpty = S.null
    creplicate = S.replicate
    czipWith = S.zipWith

{-    
instance WaveContainer V.Vector where
    cmap = V.map
    cfoldl = V.foldl'
    cempty = V.empty
    --cappend = (V.++)    --O(m+n)
    clength = V.length  --O(1)
    ccons = V.cons      --O(n) !!
    chead = V.head
    ctail = V.tail      --O(1)
    cisEmpty = V.null
-}
-- Constantes --

headerSz :: Int
headerSz = riffSz + fmtSz + hdataSz

riffSz :: Int
riffSz = sum riffS

fmtSz :: Int
fmtSz = sum fmtS

hdataSz :: Int
hdataSz = sum dataS

riffS :: [Int]    --tamaños de los campos del header RIFF
riffS = [4,4,4]

fmtS :: [Int]     --tamaños de los campos del header fmt
fmtS = [4,4,2,2,4,4,2,2]

dataS :: [Int]    --tamaños de los campos del header data
dataS = [4,4]

