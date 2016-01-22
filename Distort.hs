{-# LANGUAGE BangPatterns #-}

module Distort where

--import Control.Parallel (par,pseq)
import Control.Parallel.Strategies
import WavTypes

-- fft
import Data.List(foldl')
import Data.Complex(Complex(..),magnitude,realPart,imagPart,cis)
-- ...

import Data.Bits
import Data.Word
import Data.Conduit
import System.IO
import Control.Monad.IO.Class (liftIO)
import System.Directory (removeFile)
import qualified Data.ByteString as BS

--Misc-----------------------------

getMaxVolume :: AudioData -> Sample
getMaxVolume cs = let getMaxw :: (WaveContainer container) => container Sample -> Sample
                      --getMaxw = cfoldl (\x y -> max (abs x) (abs y)) 0
                      --getMaxw = cfoldl (\x y -> x 'seq' y 'seq' max (abs x) (abs y)) 0
                      f !x !y = max (abs x) (abs y) -- hace que se evalúe x e y antes de ejecutar f. Lo hace menos lazy para que no reviente el stack.
                      getMaxw = cfoldl f 0
                      maxvs = (parMap rseq) (\c -> getMaxw $ chData c) cs
                  in getMaxw maxvs

getAvgVolume :: AudioData -> Sample
getAvgVolume cs = let getAvgw :: (WaveContainer container) => container Sample -> Sample
                      --getAvgw w = round $ (fromIntegral $ cfoldl (\x y -> (abs x) + (abs y)) 0 w) / (fromIntegral $ clength w)
                      --getAvgw w = round $ (fromIntegral $ cfoldl (\x y -> seq (seq x y) ((abs x) + (abs y))) 0 w) / (fromIntegral $ clength w)  
                      f :: (Integral t, Integral u) => t -> u -> t
                      f !x !y = (abs $ fromIntegral x) + (abs $ fromIntegral y)
                      getAvgw w = round $ (fromIntegral $ cfoldl f 0 w) / (fromIntegral $ clength w)
                      avgvs = (parMap rseq) (\c -> getAvgw $ chData c) cs
                  in getAvgw avgvs



--SOURCE
--Obtiene un sample de cada canal.
getSamples :: WavFile -> Source IO [Sample]
getSamples wf = let sampsz = div (bitsPerSample $ fmtheader wf) 8
                    chFilesPaths = chFiles $ dataheader wf
                in do chHandles <- liftIO $ sequence $ map (\path -> openBinaryFile path ReadMode) chFilesPaths
                      res <- getSamples_ sampsz chHandles
                      liftIO $ sequence $ map removeFile chFilesPaths
                      return res
                      

                
getSamples_ :: Int -> [Handle] -> Source IO [Sample]
getSamples_ sampsz chHandles = 
    let leer h = do 
            eof <- liftIO $ hIsEOF h
            if eof
                then return Nothing
                else do str <- BS.hGet h sampsz
                        return $ Just str
    in do
        res <- liftIO $ sequence $ map leer chHandles
        let noNothing _ Nothing = False
            noNothing False _ = False
            noNothing _ _ = True
            check = foldl noNothing True res
        if check then do yield $ map (\x-> case x of Just xx -> fromByteStringtoSample sampsz xx) res
                         getSamples_ sampsz chHandles
                 else do liftIO $ sequence $ map hClose chHandles
                         return ()

--SINK                                         
--escribe un sample de cada canal, o sea que conforma un bloque en el archivo.
putSamples :: WavFile -> Sink [Sample] IO WavFile
putSamples wf = let makeTemps :: Int -> IO [(FilePath,Handle)]
                    makeTemps 0 = return []
                    makeTemps n = do tmps <- makeTemps (n-1)
                                     tmp <- openBinaryTempFile "." ("ch"++(show (nc-n))++"_.tmp")
                                     return $ tmp : tmps
                    nc = numChannels $ fmtheader wf
                in do newChFiles <- liftIO $ makeTemps nc
                      putSamples_ wf newChFiles

putSamples_ :: WavFile -> [(FilePath,Handle)] -> Sink [Sample] IO WavFile
putSamples_ wf newChFiles = do
    mx <- await
    case mx of
        Nothing -> do liftIO $ sequence $ map (hFlush.snd) newChFiles
                      chsizes <- liftIO $ sequence $ map (hFileSize.snd) newChFiles
                      let nchs  = numChannels $ fmtheader wf
                          newsz = foldl (+) 0 chsizes  --tamaño en bytes de los datos (cantidad de samples * bytes per sample * cantidad de canales)
                          oldrh = riffheader wf
                          oldfh = fmtheader wf
                          olddh = dataheader wf
                          newrh = HR { chunkID   = chunkID oldrh
                                     , chunkSize = (fromIntegral newsz) + headerSz - (riffS!!0) - (riffS!!1)
                                     , format    = format oldrh
                                     }
                          newdh = HD { chunk2ID = chunk2ID olddh
                                     , chunk2Size = fromIntegral newsz
                                     , dat = dat olddh
                                     , chFiles = map fst newChFiles
                                     }
                          newWF = W { riffheader = newrh
                                    , fmtheader  = oldfh
                                    , dataheader = newdh
                                    }
                      --liftIO $ putStrLn $ "size viejo "++(show $ chunkSize oldrh)++". sz nuevo "++(show $ chunkSize newrh)
                      liftIO $ sequence $ map (hClose.snd) newChFiles
                      return newWF
        Just samples -> let sampsz = div (bitsPerSample $ fmtheader wf) 8
                            samples' = map (fromSampletoByteString sampsz) samples
                        in do 
                            liftIO $ sequence $ map (\(s,(_,h)) -> BS.hPut h s) (zip samples' newChFiles)
                            putSamples_ wf newChFiles



fromSampletoByteString :: Int -> Sample -> BS.ByteString
fromSampletoByteString sz x =
    let loop :: Int -> Sample -> [Word8] -> [Word8]
        loop 0 x ws = ws
        loop n x ws = loop (n-1) (shiftR x 8) (((fromIntegral x)::Word8):ws)
    in BS.pack $ loop sz x []

fromByteStringtoSample :: Int->BS.ByteString->Sample
fromByteStringtoSample sz x = let xs' = BS.unpack x
                                  loop :: [Word8] -> Int -> Sample
                                  loop [] n = n 
                                  loop (x:xs) n = loop xs $ (shiftL n 8) .|. ((fromIntegral x) .&. 255) --hago & 255 para que me deje solamente los últimos 8 bits (si x es negativo me rellena con unos los primeros 24 bits)
                                  gap = (4-sz)*8 --bits que sobran a la izquierda del número. Ojo: Se asume Sample=Int de 32 bits.
                              in shiftR (shiftL (loop xs' 0) gap) gap --shifteo para mantener el signo



--reemplaza el AudioData del archivo original por el que se le pase.
putAudioData :: WavFile -> AudioData -> WavFile
putAudioData = undefined

getAudioData :: WavFile -> AudioData
getAudioData = dat . dataheader
--------------------------------

{-
--reemplaza el AudioData del archivo original por el que se le pase.
putAudioData :: WavFile -> AudioData -> WavFile
putAudioData wf a = let bps   = bitsPerSample $ fmtheader wf
                        nchs  = numChannels $ fmtheader wf
                        newsz = clength (chData $ a!!0) * (div bps 8) * nchs  --tamaño en bytes de los datos (cantidad de samples * bytes per sample * cantidad de canales)
                        oldrh = riffheader wf
                        oldfh = fmtheader wf
                        olddh = dataheader wf
                        newrh = HR { chunkID   = chunkID oldrh
                                   , chunkSize = newsz + (fromIntegral headerSz)
                                   , format    = format oldrh
                                   }
                        newdh = HD { chunk2ID = chunk2ID olddh
                                   , chunk2Size = newsz
                                   , dat = a --acá pongo el audiodata
                                   }
                    in W { riffheader = newrh
                         , fmtheader  = oldfh
                         , dataheader = newdh
                         }

getAudioData :: WavFile -> AudioData
getAudioData = dat . dataheader
--------------------------------
-}


--sube el volumen al máximo admitido por el formato sin distorsionar.
setVolMax :: WavFile -> WavFile
setVolMax wf = putAudioData wf $ setVolMaxData (getVolLimit wf) (getAudioData wf)

setVolMaxData :: Sample -> AudioData -> AudioData
setVolMaxData limit cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                             factor = ((fromIntegral limit)::Double) / maxv
                         in setVolRelData (factor * 100) cs
                         
getVolLimit :: WavFile -> Sample
getVolLimit wf = round (2 ** ((fromIntegral $ bitsPerSample (fmtheader wf)) - 1) - 1)


--control de volumen relativo a un porcentaje p del volumen máximo. VER QUE EL VALOR NO SE PASE DEL MÁXIMO ADMITIDO POR EL BPS! TODO
setVolRel :: Double -> WavFile -> WavFile
setVolRel p wf = putAudioData wf $ setVolRelData p (getAudioData wf)

setVolRelData :: Double -> AudioData -> AudioData
setVolRelData p cs = let factor = p / 100
                         modWave :: (WaveContainer container) => container Sample -> container Sample
                         modWave = cmap (\x -> round $ ((fromIntegral x)::Double) * factor)
                         modCh = (\c -> Channel { chID = chID c, chData = modWave (chData c) })
                     in (parMap rseq) modCh cs


--noise gate relativo a un porcentaje p del volumen máximo.
noiseGate :: Double -> WavFile -> WavFile
noiseGate p wf = putAudioData wf $ denoiseData p (getAudioData wf)

denoiseData :: Double -> AudioData -> AudioData
denoiseData p cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                       factor = p / 100
                       limit = round $ factor * maxv
                       modWave :: (WaveContainer container) => container Sample -> container Sample
                       modWave = cmap (\s -> if abs s<limit then 0 else s)
                       modCh = (\c -> Channel { chID = chID c, chData = modWave (chData c) })
                   in (parMap rseq) modCh cs



--hard clipping simétrico relativo a un porcentaje p del volumen máximo ("distortion").
clipRel :: Double -> WavFile -> WavFile
clipRel p wf = putAudioData wf $ clipRelData p (getAudioData wf)

clipRelData :: Double -> AudioData -> AudioData
clipRelData p cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                       factor = p / 100
                       clipval = factor * maxv
                   in clipAbsData (round clipval) cs


--hard clipping simétrico respecto a un valor absoluto v.
clipAbs :: Sample -> WavFile -> WavFile
clipAbs v wf = putAudioData wf $ clipAbsData v (getAudioData wf)

clipAbsData :: Sample -> AudioData -> AudioData
clipAbsData _ [] = []
clipAbsData v cs = let lim = abs v
                       cut = (\s -> if abs s < lim then s
                                                   else signum s*lim)
                   in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap cut (chData c) }) cs


--soft clipping ("overdrive") simétrico respecto a un porcentaje p del volumen máximo, con porcentaje de sensitividad s (s=0 equivale a hard clipping, s=100 no produce cambios).
softClipRel :: Double -> Double -> WavFile -> WavFile
softClipRel _ 100 wf = wf
softClipRel p s wf = putAudioData wf $ softClipRelData p s (getAudioData wf)

softClipRelData :: Double -> Double -> AudioData -> AudioData
softClipRelData p s cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                             factor = p / 100
                             clipval = factor * maxv
                         in softClipAbsData (round clipval) s cs


--soft clipping simétrico respecto a un valor absoluto v.
softClipAbs :: Sample -> Double -> WavFile -> WavFile
softClipAbs _ 100 wf = wf
softClipAbs v s wf = putAudioData wf $ softClipAbsData v s (getAudioData wf)

softClipAbsData :: Sample -> Double -> AudioData -> AudioData
softClipAbsData _ _ [] = []
softClipAbsData v s cs = let lim = abs v
                             pct = s / 100
                             cut = ( \s -> if abs s < lim then s
                                                          else let soft = pct * (fromIntegral (abs s-lim))
                                                               in signum s * (lim + (round soft)) )
                          in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap cut (chData c) }) cs


--compresión, equilibra los volúmenes (sube los volúmenes "bajos" y baja los "altos")
--respecto a un porcentaje p del volumen máximo, y con un porcentaje de sensitividad s (s=0 no produce cambios, s=100 aplana la onda).
compRel :: Double -> Double -> WavFile -> WavFile
compRel p s wf = putAudioData wf $ compRelData p s (getAudioData wf)

compRelData :: Double -> Double -> AudioData -> AudioData
compRelData p s cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                         factor = p / 100
                         compval = factor * maxv
                     in compAbsData (round compval) s cs

-- compresión relativa al volumen promedio con una sensitividad s.
compAvg :: Double -> WavFile -> WavFile
compAvg s wf = putAudioData wf $ compAvgData s (getAudioData wf)

compAvgData :: Double -> AudioData -> AudioData
compAvgData s cs = let avg = getAvgVolume cs
                   in compAbsData avg s cs


-- compresión respecto a un valor absoluto v con una sensitividad s.
compAbs :: Sample -> Double -> WavFile -> WavFile
compAbs v s wf = putAudioData wf $ compAbsData v s (getAudioData wf)

compAbsData :: Sample -> Double -> AudioData -> AudioData
compAbsData _ _ [] = []
compAbsData v s cs = let lim = abs v
                         pct = s / 100
                         f = \s -> signum s * ( abs s + round (fromIntegral (lim - abs s) * pct) )
                     in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap f (chData c) }) cs



-- tremolo, Speed (s, período de la onda en ms) and Depth (d, amplitud de la onda) control how fast and how much the signal varies.
-- Threshold (t) es el menor factor que se usa en la conversión (en 0 deja una onda senoidal con menor valor 0, o sea que el volumen disminuirá a 0 en algunos momentos).
-- Hace que el volumen varíe con forma de onda senoidal. Para eso hago una lista infinita de valores de una onda senoidal corrida para que sean positivos.
-- Todos los argumentos deben ser positivos. TODO
-- Precaución: puede aumentar o disminuir demasiado el volumen. Si se mete un SetVolMax después sirve bastante.
tremolo :: Double -> Double -> Double -> Bool -> WavFile -> WavFile
tremolo s d t isPanning wf = putAudioData wf $ tremoloData factoresPorCh cs
                             where cs = getAudioData wf
                                   srate = fromIntegral $ sampleRate $ fmtheader $ wf
                                   ms = 1000/srate --ms per sample
                                   s' = s/ms       --cuántas samples necesito para hacer un período
                                   pi = 3.1415926535897932384626433832795028841971693993751
                                   factor = 2*pi/s'
                                   factores = [ (d + (sin(i*factor)) * d) / 2 + t | i<-[0..] ]
                                   cslen = fromIntegral $ length cs
                                   factoresPorCh = if isPanning then [ drop (round $ s'*i/cslen) factores | i<-[0..cslen] ]
                                                                else replicate (round cslen) factores

tremoloData :: [[Double]] -> AudioData -> AudioData
tremoloData fss cs = zipWith (\fs c -> Channel { chID = chID c, chData = tremoloCh fs (chData c) }) fss cs

tremoloCh :: WaveContainer container => [Double] -> container Sample -> container Sample
tremoloCh (f:fs) ss = if cisEmpty ss then cempty
                                     else ccons (round (fromIntegral (chead ss) * f)) (tremoloCh fs (ctail ss))


-- delay, delay (ms), feedback (cantidad de veces que se repite), p (potencia, porcentaje)
-- Todos los argumentos deben ser positivos. TODO
delay :: Double -> Int -> Double -> Bool -> WavFile -> WavFile
delay d f p isEcho wf = putAudioData wf $ delayData d f p isEcho wf (getAudioData wf)

delayData :: Double -> Int -> Double -> Bool -> WavFile -> AudioData -> AudioData
delayData d f p isEcho wf cs = let srate = fromIntegral $ sampleRate $ fmtheader $ wf
                                   ms = 1000/(fromIntegral srate) --ms per sample
                                   d' = round $ d/ms --d' samples equivalen a d milisegundos.
                                   delays = [ d' * i | i<-[f,f-1..1] ]
                                   factores = if isEcho then let f_ = fromIntegral f in [ p*i / (100*(f_+1)) | i<-[1..f_] ]
                                                        else replicate f (p/100)
                                   fzip :: WaveContainer container => [Double] -> [Int] -> container Sample -> container Sample
                                   fzip _ [] c = c
                                   fzip (f':fs) (v:vs) c = let rec = fzip fs vs c
                                                               ceros = creplicate v 0
                                                               nuevo = cappend ceros (cmap (\x -> round $ fromIntegral x * f') c)
                                                               viejo = cappend rec ceros --estoy poniendo 0s de más (ya que clength rec >= clength c). Con algo menor a v alcanza, pero total zipWith descarta lo que sobra.
                                                           in czipWith (+) nuevo viejo
                               in (parMap rseq) (\c -> Channel {chID = chID c, chData = fzip factores delays (chData c)}) cs
                           








-- | /O(n^2)/. The Discrete Fourier Transform.
dft :: [Complex Double] -> [Complex Double]
dft xs = let len = length xs
          in zipWith (go len) [0..len-1] (repeat xs)
  where i = 0 :+ 1
        fi = fromIntegral
        go len k xs = foldl' (+) 0 . flip fmap [0..len-1]
          $ \n -> (xs!!n) * exp (negate(2*pi*i*fi n*fi k)/fi len)

idft :: [Complex Double] -> [Complex Double]
idft [] = []
idft xs = let n = (fromIntegral . length) xs
              (t:ts) = dft xs
          in (t/n) : ((reverse . fmap (/n)) ts)

{-
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

fff = map magnitude . fft


ifft :: [Complex Double] -> [Complex Double]
ifft [] = []
ifft xs = let n = (fromIntegral . length) xs
              (t:ts) = fft xs
          in (t/n) : ((reverse . fmap (/n)) ts)
-}
