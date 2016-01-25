{-# LANGUAGE BangPatterns #-}

module Distort where

--import Control.Parallel (par,pseq)
import Control.Parallel.Strategies
import WavTypes

import Data.Bits
import Data.Word
import Data.Conduit
import System.IO
import Control.Monad.IO.Class (liftIO)
import System.Directory (removeFile)
import qualified Data.ByteString as BS
import qualified Data.List as L

--Misc-----------------------------

--esto puede ser sink
getMaxVolume :: AudioData -> Sample
getMaxVolume cs = let getMaxw :: (WaveContainer container) => container Sample -> Sample
                      --getMaxw = cfoldl (\x y -> max (abs x) (abs y)) 0
                      --getMaxw = cfoldl (\x y -> x 'seq' y 'seq' max (abs x) (abs y)) 0
                      f !x !y = max (abs x) (abs y) -- hace que se evalúe x e y antes de ejecutar f. Lo hace menos lazy para que no reviente el stack.
                      getMaxw = cfoldl f 0
                      maxvs = (parMap rseq) (\c -> getMaxw $ chData c) cs
                  in getMaxw maxvs


getMaxVolume2 :: WavFile -> IO Sample
getMaxVolume2 wf = getSamples wf $$ getMaxVolume2_ 0

getMaxVolume2_ :: Sample -> Sink [Sample] IO Sample
getMaxVolume2_ !m = do
    x <- await
    case x of
        Nothing -> return m
        Just samples -> let f !x !y = max (abs x) (abs y)
                        in getMaxVolume2_ $ foldl f m samples 
            
                            
                            
--esto puede ser sink
getAvgVolume :: AudioData -> Sample
getAvgVolume cs = let getAvgw :: (WaveContainer container) => container Sample -> Sample
                      --getAvgw w = round $ (fromIntegral $ cfoldl (\x y -> (abs x) + (abs y)) 0 w) / (fromIntegral $ clength w)
                      --getAvgw w = round $ (fromIntegral $ cfoldl (\x y -> seq (seq x y) ((abs x) + (abs y))) 0 w) / (fromIntegral $ clength w)  
                      f :: (Integral t, Integral u) => t -> u -> t
                      f !x !y = (abs $ fromIntegral x) + (abs $ fromIntegral y)
                      getAvgw w = round $ (fromIntegral $ cfoldl f 0 w) / (fromIntegral $ clength w)
                      avgvs = (parMap rseq) (\c -> getAvgw $ chData c) cs
                  in getAvgw avgvs


getAvgVolume2 :: WavFile -> IO Sample
getAvgVolume2 wf = getSamples wf $$ getAvgVolume2_ 0 0

getAvgVolume2_ :: Integer -> Integer -> Sink [Sample] IO Sample
getAvgVolume2_ !tot !cant = do
    x <- await
    case x of
        Nothing -> return $ fromIntegral $ div tot cant
        Just samples -> let f !x !y = (fromIntegral $ abs x) + (fromIntegral $ abs y)
                        in getAvgVolume2_ (foldl f tot samples) (fromIntegral (length samples) + cant)


--SOURCE
--Obtiene un sample de cada canal.
getSamples :: WavFile -> Source IO [Sample]
getSamples wf = let sampsz = div (bitsPerSample $ fmtheader wf) 8
                    chFilesPaths = chFiles $ dataheader wf
                in do chHandles <- liftIO $ sequence $ map (\path -> openBinaryFile path ReadMode) chFilesPaths
                      res <- getSamples_ sampsz chHandles
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
putSamples wf = let nc = numChannels $ fmtheader wf
                in do newChFiles <- liftIO $ makeTemps nc nc
                      putSamples_ wf newChFiles

putSamples_ :: WavFile -> [(FilePath,Handle)] -> Sink [Sample] IO WavFile
putSamples_ wf newChFiles = do
    mx <- await
    case mx of
        Nothing -> do liftIO $ sequence $ map (hFlush.snd) newChFiles
                      newWF <- liftIO $ updateWavFile wf newChFiles
                      liftIO $ sequence $ map (hClose.snd) newChFiles
                      let oldChPaths = chFiles $ dataheader wf
                      liftIO $ sequence $ map removeFile oldChPaths
                      return newWF
        Just samples -> let sampsz = div (bitsPerSample $ fmtheader wf) 8
                            samples' = map (fromSampletoByteString sampsz) samples
                        in do 
                            liftIO $ sequence $ map (\(s,(_,h)) -> BS.hPut h s) (zip samples' newChFiles)
                            putSamples_ wf newChFiles


updateWavFile :: WavFile -> [(FilePath,Handle)] -> IO WavFile
updateWavFile wf newChFiles = do
    chsizes <- liftIO $ sequence $ map (hFileSize.snd) newChFiles
    let nchs  = numChannels $ fmtheader wf
        newsz = foldl (+) 0 chsizes  --tamaño en bytes de los datos (puede haber cambiado con algún delay o echo).
        oldrh = riffheader wf
        oldfh = fmtheader wf
        olddh = dataheader wf
        newrh = HR { chunkID   = chunkID oldrh
                   , chunkSize = (fromIntegral newsz) + headerSz - (riffS!!0) - (riffS!!1) --ver WavTypes.hs
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
    return newWF


makeTemps :: Int -> Int -> IO [(FilePath,Handle)]
makeTemps 0 _  = return []
makeTemps n nc = do tmps <- makeTemps (n-1) nc
                    tmp <- openBinaryTempFile "." ("ch"++(show (nc-n))++"_.tmp")
                    return $ tmp : tmps


fromSampletoByteString :: Int -> Sample -> BS.ByteString
fromSampletoByteString sz x =
    let loop :: Int -> Sample -> [Word8] -> [Word8]
        loop 0 x ws = ws
        loop n x ws = loop (n-1) (shiftR x 8) (((fromIntegral x)::Word8):ws)
        maxlim = shiftL 1 (sz*8-1) - 1
        minlim = -maxlim - 1
    in BS.pack $ reverse $ if x>maxlim then loop sz maxlim [] --reverse porque es little endian!
                           else if x<minlim then loop sz minlim []
                           else if sz==1 then loop sz (x+128) [] --si es 8 bits se almacena como unsigned!
                           else loop sz x []
    

fromByteStringtoSample :: Int->BS.ByteString->Sample
fromByteStringtoSample sz x = let xs' = reverse $ BS.unpack x --reverse porque es little endian!
                                  loop :: [Word8] -> Int -> Sample
                                  loop [] n = n 
                                  loop (x:xs) n = loop xs $ (shiftL n 8) .|. ((fromIntegral x) .&. 255) --hago & 255 para que me deje solamente los últimos 8 bits (si x es negativo me rellena con unos los primeros 24 bits)
                                  gap = (4-sz)*8 --bits que sobran a la izquierda del número. Ojo: Se asume Sample=Int de 32 bits.
                                  res = shiftR (shiftL (loop xs' 0) gap) gap --shifteo para mantener el signo
                              in if sz==1 then (res-128) else res --si es 8 bits se almacena como unsigned!



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
                                   , chFiles = []
                                   }
                    in W { riffheader = newrh
                         , fmtheader  = oldfh
                         , dataheader = newdh
                         }

getAudioData :: WavFile -> AudioData
getAudioData = dat . dataheader
--------------------------------


{-
Los efectos que usan valores absoluto no necesitan ni siquiera usar un sink aparte antes,
les alcanza con hacer algo tipo GETSAMPLES $$ efecto_absoluto =$ PUTSAMPLES
-}


--esto TENDRÍA QUE SER ONDA x<-GETMAXVOLUME; GETSAMPLES $$ SETVOL x =$ PUTSAMPLES
--sube el volumen al máximo admitido por el formato sin distorsionar.
setVolMax :: WavFile -> WavFile
setVolMax wf = putAudioData wf $ setVolMaxData (getVolLimit wf) (getAudioData wf)

setVolMaxData :: Sample -> AudioData -> AudioData
setVolMaxData limit cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                             factor = ((fromIntegral limit)::Double) / maxv
                         in setVolRelData (factor * 100) cs
                         
getVolLimit :: WavFile -> Sample
getVolLimit wf = shiftL 1 (bitsPerSample (fmtheader wf) - 1) - 1


setVolMax2 :: WavFile -> IO WavFile
setVolMax2 wf = do maxv' <- getMaxVolume2 wf
                   let maxv = (fromIntegral maxv')::Double
                       limit = getVolLimit wf
                       factor = ((fromIntegral limit)::Double) / maxv
                   setVolRel2 (factor*100) wf                    


--control de volumen relativo a un porcentaje p del volumen máximo. VER QUE EL VALOR NO SE PASE DEL MÁXIMO ADMITIDO POR EL BPS! ya se chequea em fromSampletoByteString
setVolRel :: Double -> WavFile -> WavFile
setVolRel p wf = putAudioData wf $ setVolRelData p (getAudioData wf)

setVolRelData :: Double -> AudioData -> AudioData
setVolRelData p cs = let factor = p / 100
                         modWave :: (WaveContainer container) => container Sample -> container Sample
                         modWave = cmap (\x -> round $ ((fromIntegral x)::Double) * factor)
                         modCh = (\c -> Channel { chID = chID c, chData = modWave (chData c) })
                     in (parMap rseq) modCh cs


setVolRel2 :: Double -> WavFile -> IO WavFile
setVolRel2 p wf = let f s = round $ (fromIntegral s) * p/100
                  in getSamples wf $$ mapSamples f =$ putSamples wf



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


noiseGate2 :: Double -> WavFile -> IO WavFile
noiseGate2 p wf = do
    maxv' <- getMaxVolume2 wf
    let maxv = (fromIntegral maxv')::Double
        factor = p / 100
        limit = round $ factor * maxv
        f s = if abs s<limit then 0 else s
    getSamples wf $$ mapSamples f =$ putSamples wf



--hard clipping simétrico relativo a un porcentaje p del volumen máximo ("distortion").
clipRel :: Double -> WavFile -> WavFile
clipRel p wf = putAudioData wf $ clipRelData p (getAudioData wf)

clipRelData :: Double -> AudioData -> AudioData
clipRelData p cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                       factor = p / 100
                       clipval = factor * maxv
                   in clipAbsData (round clipval) cs


clipRel2 :: Double -> WavFile -> IO WavFile
clipRel2 p wf = do
    maxv' <- getMaxVolume2 wf
    let maxv = (fromIntegral maxv')::Double
        factor = p / 100
        clipval = round $ factor * maxv
    clipAbs2 clipval wf


--hard clipping simétrico respecto a un valor absoluto v.
clipAbs :: Sample -> WavFile -> WavFile
clipAbs v wf = putAudioData wf $ clipAbsData v (getAudioData wf)

clipAbsData :: Sample -> AudioData -> AudioData
clipAbsData _ [] = []
clipAbsData v cs = let lim = abs v
                       cut = (\s -> if abs s < lim then s
                                                   else signum s*lim)
                   in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap cut (chData c) }) cs


clipAbs2 :: Sample -> WavFile -> IO WavFile
clipAbs2 v wf = let lim = abs v
                    f :: Sample -> Sample
                    f s = if abs s < lim then s else signum s*lim
                in getSamples wf $$ mapSamples f =$ putSamples wf


mapSamples :: (Sample -> Sample) -> Conduit [Sample] IO [Sample]
mapSamples f = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just samples -> do 
            yield $ map f samples
            mapSamples f



--soft clipping ("overdrive") simétrico respecto a un porcentaje p del volumen máximo, con porcentaje de sensitividad s (s=0 equivale a hard clipping, s=100 no produce cambios).
softClipRel :: Double -> Double -> WavFile -> WavFile
softClipRel p s wf = putAudioData wf $ softClipRelData p s (getAudioData wf)

softClipRelData :: Double -> Double -> AudioData -> AudioData
softClipRelData p s cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                             factor = p / 100
                             clipval = factor * maxv
                         in softClipAbsData (round clipval) s cs


softClipRel2 :: Double -> Double -> WavFile -> IO WavFile
softClipRel2 p s wf = do
    maxv' <- getMaxVolume2 wf
    let maxv = (fromIntegral maxv')::Double
        factor = p / 100
        clipval = round $ factor * maxv
    softClipAbs2 clipval s wf



--soft clipping simétrico respecto a un valor absoluto v.
softClipAbs :: Sample -> Double -> WavFile -> WavFile
softClipAbs v s wf = putAudioData wf $ softClipAbsData v s (getAudioData wf)

softClipAbsData :: Sample -> Double -> AudioData -> AudioData
softClipAbsData _ _ [] = []
softClipAbsData v s cs = let lim = abs v
                             pct = s / 100
                             cut = ( \s -> if abs s < lim then s
                                                          else let soft = pct * (fromIntegral (abs s-lim))
                                                               in signum s * (lim + (round soft)) )
                          in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap cut (chData c) }) cs


softClipAbs2 :: Sample -> Double -> WavFile -> IO WavFile
softClipAbs2 v s wf = let lim = abs v
                          factor = s/100
                          f :: Sample -> Sample
                          f s = if abs s < lim then s else let soft = factor * (fromIntegral (abs s-lim))
                                                           in signum s * (lim + (round soft))
                      in getSamples wf $$ mapSamples f =$ putSamples wf


--compresión, equilibra los volúmenes (sube los volúmenes "bajos" y baja los "altos")
--respecto a un porcentaje p del volumen máximo, y con un porcentaje de sensitividad s (s=0 no produce cambios, s=100 aplana la onda).
compRel :: Double -> Double -> WavFile -> WavFile
compRel p s wf = putAudioData wf $ compRelData p s (getAudioData wf)

compRelData :: Double -> Double -> AudioData -> AudioData
compRelData p s cs = let maxv = (fromIntegral $ getMaxVolume cs)::Double
                         factor = p / 100
                         compval = factor * maxv
                     in compAbsData (round compval) s cs


compRel2 :: Double -> Double -> WavFile -> IO WavFile
compRel2 p s wf = do
    maxv <- getMaxVolume2 wf
    let factor = p/100
        maxvd = (fromIntegral maxv)::Double
        compval = round $ factor*maxvd
    compAbs2 compval s wf


-- compresión relativa al volumen promedio con una sensitividad s.
compAvg :: Double -> WavFile -> WavFile
compAvg s wf = putAudioData wf $ compAvgData s (getAudioData wf)

compAvgData :: Double -> AudioData -> AudioData
compAvgData s cs = let avg = getAvgVolume cs
                   in compAbsData avg s cs


compAvg2 :: Double -> WavFile -> IO WavFile
compAvg2 s wf = do
    avg <- getAvgVolume2 wf
    compAbs2 avg s wf


-- compresión respecto a un valor absoluto v con una sensitividad s.
compAbs :: Sample -> Double -> WavFile -> WavFile
compAbs v s wf = putAudioData wf $ compAbsData v s (getAudioData wf)

compAbsData :: Sample -> Double -> AudioData -> AudioData
compAbsData _ _ [] = []
compAbsData v s cs = let lim = abs v
                         pct = s / 100
                         f = \s -> signum s * ( abs s + round (fromIntegral (lim - abs s) * pct) )
                     in (parMap rseq) (\c->Channel { chID = chID c, chData = cmap f (chData c) }) cs



compAbs2 :: Sample -> Double -> WavFile -> IO WavFile
compAbs2 v s wf = let lim = abs v
                      factor = s/100
                      f s = signum s * ( abs s + round (fromIntegral (lim - abs s) * factor) )
                  in getSamples wf $$ mapSamples f =$ putSamples wf


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



tremolo2 :: Double -> Double -> Double -> Bool -> WavFile -> IO WavFile
tremolo2 s d t isPanning wf = let chFilePaths = chFiles $ dataheader wf
                                  srate = fromIntegral $ sampleRate $ fmtheader $ wf
                                  ms = 1000/srate --ms per sample
                                  s' = s/ms       --cuántas samples necesito para hacer un período
                                  pi = 3.1415926535897932384626433832795028841971693993751
                                  factor = 2*pi/s'
                                  nchs = fromIntegral $ numChannels $ fmtheader wf
                                  f :: Double -> Double -> Double
                                  f nc i = let offset = s'*nc/nchs --para el panning. Si isPanning=False entonces offset=0, o sea que no depende del número del canal, para todos los canales es igual.
                                           in (d + (sin((i+offset)*factor)) * d) / 2 + t
                                  factoresPorCh = if isPanning then [ [f nc i | i<-[0..]] | nc<-[0..nchs-1] ]
                                                               else [ [f 0  i | i<-[0..]] | nc<-[0..nchs-1] ]
                              in getSamples wf $$ tremolo2_ factoresPorCh =$ putSamples wf

tremolo2_ :: [[Double]] -> Conduit [Sample] IO [Sample]
tremolo2_ factoress = do
    mx <- await
    case mx of
        Nothing -> return ()
        Just samples -> 
            let factores = map head factoress
                fys = zip factores samples 
                fun (f,s) = round $ (fromIntegral s) * f
            in do
                yield $ map fun fys
                tremolo2_ $ map tail factoress

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
                           



{-

-- fft
import Data.List(foldl')
import Data.Complex(Complex(..),magnitude,realPart,imagPart,cis)
-- ...


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
-}
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
