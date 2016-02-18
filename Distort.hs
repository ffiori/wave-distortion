{-# LANGUAGE BangPatterns #-}

module Distort
  ( setVolMax
  , setVolRel
  , noiseGate
  , clipRel
  , clipAbs
  , softClipRel
  , softClipAbs
  , compRel
  , compAvg
  , compAbs
  , tremolo
  , delay
  ) where

import WavTypes
import Utilities (safelyRemoveFile, makeTemps, fromByteStringtoSample, fromSampletoByteString)

import qualified Data.ByteString as BS
import Data.Bits
import Data.Int
import Data.Word
import Data.Conduit
import System.IO
import Control.Monad.IO.Class (liftIO)
import System.Directory (removeFile, doesFileExist)

import qualified Control.Exception as E
import System.IO.Error

import Control.Monad.Trans.Resource -- necesario para usar bracketP y poder manejar excepciones dentro de conduit.
type RIO = ResourceT IO

--Misc--------------------------------------------------------------------------
--ejecuta action y si es que falla borra los temporales de wf y los que hay en newChs
safelyDo :: RIO WavFile -> WavFile -> [(FilePath,Handle)] -> IO WavFile
safelyDo action wf newChs = runResourceT action `E.catch` catcher
  where 
    catcher :: E.SomeException -> IO WavFile
    catcher e = do --borra los canales del WavFile y los temporales en newChs
      sequence $ map (hClose.snd) newChs
      sequence $ map safelyRemoveFile $ (map fst newChs)++(chFiles $ dataheader wf)
      E.throw e


--devuelve el máximo valor que puede tener un sample de bitsPerSample bits
getVolLimit :: WavFile -> Sample
getVolLimit wf = shiftL 1 (bps - 1) - 1
    where bps = fromIntegral $ bitsPerSample $ fmtheader wf


getMaxVolume :: WavFile -> IO Sample
getMaxVolume wf = runResourceT (getSamples wf $$ getMaxVolume_ 0) `E.catch` catcher
  where catcher :: E.SomeException -> IO Sample
        catcher e = do sequence $ map safelyRemoveFile (chFiles $ dataheader wf)
                       E.throw e

getMaxVolume_ :: Sample -> Sink [Sample] RIO Sample
getMaxVolume_ !m = do
  x <- await
  case x of
    Nothing -> return m
    Just samples -> let f !x !y = max (abs x) (abs y)
                    in getMaxVolume_ $ foldl f m samples 


getAvgVolume :: WavFile -> IO Sample
getAvgVolume wf = runResourceT (getSamples wf $$ getAvgVolume_ 0 0) `E.catch` catcher
  where catcher :: E.SomeException -> IO Sample
        catcher e = do sequence $ map safelyRemoveFile (chFiles $ dataheader wf)
                       E.throw e

getAvgVolume_ :: Integer -> Integer -> Sink [Sample] RIO Sample
getAvgVolume_ !tot !cant = do
  x <- await
  case x of
    Nothing -> return $ fromIntegral $ div tot cant
    Just samples -> 
      let f :: Integer -> Sample -> Integer
          f !x !y = x + (fromIntegral $ abs y)
      in getAvgVolume_ (foldl f tot samples) (fromIntegral (length samples) + cant)


--SOURCE
--Obtiene un sample de cada canal.
getSamples :: WavFile -> Source RIO [Sample]
getSamples wf =
  let sampsz = div (bitsPerSample $ fmtheader wf) 8
      chFilesPaths = chFiles $ dataheader wf
  in bracketP
     (sequence $ map (\path -> openBinaryFile path ReadMode) chFilesPaths)
     (\chHandles -> do sequence $ map hClose chHandles
                       return ())
     (\chHandles -> getSamples_ sampsz chHandles)

getSamples_ :: Int32 -> [Handle] -> Source RIO [Sample]
getSamples_ sampsz chHandles = 
  let leer :: Handle -> IO (Maybe BS.ByteString)
      leer h = do 
        eof <- liftIO $ hIsEOF h
        if eof
          then return Nothing
          else do str <- BS.hGet h (fromIntegral sampsz)
                  return $ Just str
  in do
    res <- liftIO $ sequence $ map leer chHandles
    case sequence res of
      Just ss -> do yield $ map (fromByteStringtoSample sampsz) ss
                    getSamples_ sampsz chHandles
      Nothing -> return ()

--SINK                                         
--escribe un sample en cada canal.
putSamples :: WavFile -> [(FilePath,Handle)] -> Sink [Sample] RIO WavFile
putSamples wf newChFiles = do
  mx <- await
  case mx of
    Nothing -> do 
      liftIO $ sequence $ map (hFlush.snd) newChFiles
      newWF <- liftIO $ updateWavFile wf newChFiles
      liftIO $ sequence $ map (hClose.snd) newChFiles
      let oldChPaths = chFiles $ dataheader wf
      liftIO $ sequence $ map removeFile oldChPaths
      return newWF
    Just samples -> 
      let sampsz = div (bitsPerSample $ fmtheader wf) 8
          samples' = map (fromSampletoByteString sampsz) samples
      in do 
        liftIO $ sequence $ map (\(s,(_,h)) -> BS.hPut h s) (zip samples' newChFiles)
        putSamples wf newChFiles


updateWavFile :: WavFile -> [(FilePath,Handle)] -> IO WavFile
updateWavFile wf newChFiles = do
  chsizes <- liftIO $ sequence $ map (hFileSize.snd) newChFiles
  let nchs  = numChannels $ fmtheader wf
      newsz = foldl (+) 0 chsizes  --tamaño en bytes de los datos (puede haber cambiado con algún delay o echo).
      oldrh = riffheader wf
      oldfh = fmtheader wf
      olddh = dataheader wf
      finalHeaderSize = if audioFormat oldfh == -2 then headerExtSz else headerSz --ver WavTypes.hs
      newrh = HR { chunkID   = chunkID oldrh
                 , chunkSize = (fromIntegral newsz) + (fromIntegral $ - (riffS!!0) - (riffS!!1) + finalHeaderSize)
                 , format    = format oldrh
                 }
      newdh = HD { chunk2ID = chunk2ID olddh
                 , chunk2Size = fromIntegral newsz
                 , chFiles = map fst newChFiles
                 }
      newWF = W { riffheader = newrh
                , fmtheader  = oldfh
                , dataheader = newdh
                }
  return newWF


-- mapea la función f a cada sample de los canales.
mapSamples :: (Sample -> Sample) -> Conduit [Sample] RIO [Sample]
mapSamples f = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just samples -> do 
      yield $ map f samples
      mapSamples f
--------------------------------------------------------------------------------

{-
Los efectos que usan valores absolutos no necesitan ni siquiera usar un sink aparte antes,
les alcanza con hacer algo tipo GETSAMPLES $$ efecto_absoluto =$ PUTSAMPLES
-}

--sube el volumen al máximo admitido por el formato sin distorsionar.
setVolMax :: WavFile -> IO WavFile
setVolMax wf = do
  maxv' <- getMaxVolume wf
  let maxv = (fromIntegral maxv')::Double
      limit = getVolLimit wf
      factor = ((fromIntegral limit)::Double) / maxv
  setVolRel (factor*100) wf                    


--control de volumen relativo a un porcentaje p del volumen máximo.
setVolRel :: Double -> WavFile -> IO WavFile
setVolRel p wf = 
  let f :: Sample -> Sample
      f s = round $ (fromIntegral s) * p/100
  in do newChs <- makeTemps wf
        safelyDo (getSamples wf $$ mapSamples f =$ putSamples wf newChs) wf newChs 


--noise gate relativo a un porcentaje p del volumen máximo.
noiseGate :: Double -> WavFile -> IO WavFile
noiseGate p wf = do
  maxv' <- getMaxVolume wf
  let maxv = (fromIntegral maxv')::Double
      factor = p / 100
      limit = round $ factor * maxv
      f :: Sample -> Sample
      f s = if abs s<limit then 0 else s
  newChs <- makeTemps wf
  safelyDo (getSamples wf $$ mapSamples f =$ putSamples wf newChs) wf newChs


--hard clipping simétrico relativo a un porcentaje p del volumen máximo ("distortion").
clipRel :: Double -> WavFile -> IO WavFile
clipRel p wf = do
  maxv' <- getMaxVolume wf
  let maxv = (fromIntegral maxv')::Double
      factor = p / 100
      clipval = round $ factor * maxv
  clipAbs clipval wf


--hard clipping simétrico respecto a un valor absoluto v.
clipAbs :: Sample -> WavFile -> IO WavFile
clipAbs v wf =
  let lim = abs v
      f :: Sample -> Sample
      f s = if abs s < lim then s else signum s*lim
  in do newChs <- makeTemps wf
        safelyDo (getSamples wf $$ mapSamples f =$ putSamples wf newChs) wf newChs


--soft clipping ("overdrive") simétrico respecto a un porcentaje p del volumen máximo, con porcentaje de sensitividad s (s=0 equivale a hard clipping, s=100 no produce cambios).
softClipRel :: Double -> Double -> WavFile -> IO WavFile
softClipRel p s wf = do
  maxv' <- getMaxVolume wf
  let maxv = (fromIntegral maxv')::Double
      factor = p / 100
      clipval = round $ factor * maxv
  softClipAbs clipval s wf


--soft clipping simétrico respecto a un valor absoluto v.
softClipAbs :: Sample -> Double -> WavFile -> IO WavFile
softClipAbs v s wf =
  let lim = abs v
      factor = s/100
      f :: Sample -> Sample
      f s = if abs s < lim 
              then s
              else let soft = factor * (fromIntegral (abs s-lim))
                   in signum s * (lim + (round soft))
  in do newChs <- makeTemps wf
        safelyDo (getSamples wf $$ mapSamples f =$ putSamples wf newChs) wf newChs


--compresión, equilibra los volúmenes (sube los volúmenes "bajos" y baja los "altos")
--respecto a un porcentaje p del volumen máximo, y con un porcentaje de sensitividad s (s=0 no produce cambios, s=100 aplana la onda).
compRel :: Double -> Double -> WavFile -> IO WavFile
compRel p s wf = do
  maxv <- getMaxVolume wf
  let factor = p/100
      maxvd = (fromIntegral maxv)::Double
      compval = round $ factor*maxvd
  compAbs compval s wf


-- compresión relativa al volumen promedio con una sensitividad s.
compAvg :: Double -> WavFile -> IO WavFile
compAvg s wf = do
  avg <- getAvgVolume wf
  compAbs avg s wf


-- compresión respecto a un valor absoluto v con una sensitividad s.
compAbs :: Sample -> Double -> WavFile -> IO WavFile
compAbs v s wf =
  let lim = abs v
      factor = s/100
      f :: Sample -> Sample
      f s = signum s * ( abs s + round (fromIntegral (lim - abs s) * factor) )
  in do newChs <- makeTemps wf
        safelyDo (getSamples wf $$ mapSamples f =$ putSamples wf newChs) wf newChs


-- tremolo: s = Speed (período de la onda en ms), d = Depth (amplitud de la onda) controls how fast and how much the signal varies.
-- Threshold (t) es el menor factor que se usa en la conversión (en 0 deja una onda senoidal con menor valor 0, o sea que el volumen disminuirá a 0 en algunos momentos).
-- Hace que el volumen varíe con forma de onda senoidal. Para eso tomo valores de una onda senoidal corrida para que sean positivos.
-- Precaución: puede aumentar o disminuir demasiado el volumen. Si se mete un SetVolMax después sirve bastante.
-- isPanning = True => el sonido va pasando de un canal a otro en potencia.
tremolo :: Double -> Double -> Double -> Bool -> WavFile -> IO WavFile
tremolo s d t isPanning wf =
  if s<0 || d<0 || t<0
    then error $ "Parámetro negativo en Tremolo o Panning." -- Todos los argumentos deben ser positivos.
    else 
      let chFilePaths = chFiles $ dataheader wf
          srate = fromIntegral $ sampleRate $ fmtheader $ wf
          ms = 1000/srate --ms per sample
          s' = s/ms       --cuántas samples necesito para hacer un período
          pi = 3.1415926535897932384626433832795028841971693993751
          factor = 2*pi/s'
          nchs = numChannels $ fmtheader wf
          f :: Double -> Double -> Double
          f nc i = let offset = s'*nc/(fromIntegral nchs) --para el panning. Si isPanning=False entonces offset=0, o sea que no depende del número del canal, para todos los canales es igual.
                   in (d + (sin((i+offset)*factor)) * d) / 2 + t
      in do 
        newChs <- makeTemps wf
        safelyDo (getSamples wf $$ tremolo_ f 0 isPanning nchs =$ putSamples wf newChs) wf newChs

tremolo_ :: (Double -> Double -> Double) -> Integer -> Bool -> Int32 -> Conduit [Sample] RIO [Sample]
tremolo_ f !i isPanning nchs = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just samples ->
      let factores = if isPanning 
                       then [ f (fromIntegral nc) (fromIntegral i) | nc<-[0..nchs-1] ]
                       else [ f 0  (fromIntegral i) | nc<-[0..nchs-1] ]
          fys = zip factores samples 
          fun (!f,!s) = round $ (fromIntegral s) * f
          modSamples = map fun fys
      in do
          yield $! modSamples
          tremolo_ f (i+1) isPanning nchs


-- delay: d = delay (ms), f = feedback (cantidad de veces que se repite), p = potencia o porcentaje de volumen.
-- isEcho = True => el delay se va disminuyendo linealmente en potencia
delay :: Double -> Int -> Double -> Bool -> WavFile -> IO WavFile
delay d f p isEcho wf = 
  if d<0 || f<0 || p<0
    then error $ "Parámetro negativo en Delay o Echo." -- Todos los argumentos deben ser positivos.
    else 
      let 
        srate = fromIntegral $ sampleRate $ fmtheader $ wf
        ms = 1000/(fromIntegral srate) --ms per sample
        d' = round $ d/ms --d' samples equivalen a d milisegundos.
        delays = [ d' * i | i<-[f,f-1..1] ]
        factores = if isEcho 
                     then let f_ = fromIntegral f in [ p*i / (100*f_) | i<-[1..f_] ]
                     else replicate f (p/100)
        nchs = numChannels $ fmtheader wf
        sampsz = div (bitsPerSample $ fmtheader wf) 8
        
        --versión segura para crear los temporales de echoes
        makeChsEch :: Int32 -> Int -> [[(FilePath,Handle)]] -> IO [[(FilePath,Handle)]]
        makeChsEch 0  _ acum = return acum
        makeChsEch ch n acum = E.bracketOnError
                               (makeEchoes (ch-1) (n-1))
                               (\ es -> sequence $ map (safelyRemoveFile.fst) (concat (es:acum)))
                               (\ es -> makeChsEch (ch-1) n (es:acum))
        
        makeEchoes :: Int32 -> Int -> IO [(FilePath,Handle)]
        makeEchoes ch (-1) = return []
        makeEchoes ch i = E.bracketOnError
                          (openBinaryTempFile "." ("ch"++(show ch)++"echo"++(show i)++"_.tmp")) 
                          (\ (path,_) -> sequence $ map safelyRemoveFile (path : (chFiles $ dataheader wf)) )
                          (\ tmp -> do tmps <- makeEchoes ch (i-1)
                                       return $ tmp:tmps )
      in do
        -- echoes = [ [ch0echo0, ch0echo1,...], [ch1echo0,ch1echo1,...], ...], o sea :: [[(FilePath,Handle)]]
        -- Por cada feedback creo un archivo nuevo.
        echoes <- makeChsEch nchs f [] --sequence $ [ sequence [openBinaryTempFile "." ("ch"++(show ch)++"echo"++(show i)++"_.tmp") | i<-[0..f-1]] | ch<-[0..nchs-1] ]
        
        -- Pongo ceros en cada archivo dependiendo del delay y del nº de feedback
        let aux es = sequence $ map (\(dx,(_,h)) -> BS.hPut h (BS.concat $ replicate dx (fromSampletoByteString sampsz 0))) es
        (sequence $ map aux (map (zip delays) echoes))  `E.catch` (catcher $ concat echoes)

        newChs <- makeTemps wf
        wf' <- safelyDo (getSamples wf $$ armarOndas (map (zip factores) echoes) sampsz =$ putSamples wf newChs) wf (newChs++(concat echoes))
        sequence $ map (hClose.snd) $ concat echoes
        
        -- Sumo las ondas que generé y las guardo en un mismo archivo (hago esto para cada canal)
        let originalFiles = chFiles $ dataheader wf'
            echoFiles = map (map fst) echoes
            todas = map (\(o,es)->(o:es)) (zip originalFiles echoFiles)
        newChs' <- makeTemps wf'
        wffinal <- safelyDo (juntarOndas sampsz todas $$ putSamples wf' newChs') wf' (newChs'++(concat echoes))
        
        sequence $ map (removeFile.fst) $ concat echoes
        return wffinal
    where
      catcher :: [(FilePath,Handle)] -> E.SomeException -> IO [[()]]
      catcher echoes e = do
        sequence $ map (hClose.snd) echoes
        sequence $ map (safelyRemoveFile.fst) echoes
        sequence $ map safelyRemoveFile (chFiles $ dataheader wf)
        E.throw e

armarOndas :: [[ (Double, (FilePath,Handle)) ]] -> Int32 -> Conduit [Sample] RIO [Sample]
armarOndas efs sampsz = do  -- efs :: [(factor, (path,handle))]
  mx <- await
  case mx of
    Nothing -> return ()
    Just samples ->
      let zs = zip efs samples
          aux (es,s) = sequence $ map (\(f,(_,h)) -> BS.hPut h (fromSampletoByteString sampsz $ round $ fromIntegral s * f)) es
      in do
        liftIO $ sequence $ map aux zs
        yield samples
        armarOndas efs sampsz

juntarOndas :: Int32 -> [[FilePath]] -> Source RIO [Sample]
juntarOndas sampsz chEchos = bracketP
  (sequence $ map (\es -> sequence $ map (\path -> openBinaryFile path ReadMode) es) chEchos)
  (\chHandless -> do sequence $ map hClose (concat chHandless)
                     return ()) --cierro los handles al terminar, aún cuando surja una excepción.
  (\chHandless -> juntarOndas_ sampsz chHandless)
    
juntarOndas_ :: Int32 -> [[Handle]] -> Source RIO [Sample]
juntarOndas_ sampsz chHandless =
  let leer :: Handle -> IO (Maybe Sample)
      leer h = do
        eof <- liftIO $ hIsEOF h
        if eof
          then return Nothing
          else do str <- BS.hGet h (fromIntegral sampsz)
                  return $ Just (fromByteStringtoSample sampsz str)
  in do
    presamples <- liftIO $ sequence $ map (\hs -> sequence $ map leer hs) chHandless -- :: [[Maybe Sample]]
    let samples = map (foldl aux Nothing) presamples
        aux :: Maybe Sample -> Maybe Sample -> Maybe Sample
        aux Nothing Nothing = Nothing
        aux Nothing (Just x) = Just x
        aux (Just x) Nothing = Just x
        aux (Just x) (Just y) = Just (x+y)
    case sequence samples of
      Just ss -> do yield ss
                    juntarOndas_ sampsz chHandless
      Nothing -> return ()

{-
    Por cada feedback creo un archivo nuevo con tantos ceros como tenga en delay
    de ése, y después los uno todos con suma, dejando la longitud del más largo.
    
    Después con un conduit hago yield de lo mismo que le llega, y que
    ya tenga creados los archivos con los ceros, entonces lo único que tiene que
    hacer es hPut (handle_archivo(i)) (fromBStoSample $ sample*factor(i)).
-}
