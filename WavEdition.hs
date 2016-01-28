{-# LANGUAGE BangPatterns #-}
module WavEdition where

import WavTypes
import WavRead
import WavWrite
import Distort

import Control.Monad.State
import Data.Conduit

data Efecto = SetVolMax
             | SetVolRel Double
             | NoiseGate Double
             | ClipRel Double
             | ClipAbs Sample
             | SoftClipRel Double Double
             | SoftClipAbs Sample Double
             | CompRel Double Double
             | CompAvg Double
             | CompAbs Sample Double
             | Tremolo Double Double Double
             | Panning Double Double Double
             | Delay Double Int Double
             | Echo Double Int Double


-- Convierte un elemento de la representación en deep embedding a la función correspondiente.
toFunc :: Efecto -> WavFile -> IO WavFile

toFunc SetVolMax = setVolMax
toFunc (SetVolRel v) = setVolRel v
toFunc (NoiseGate x) = noiseGate x
toFunc (ClipRel p) = clipRel p
toFunc (ClipAbs v) = clipAbs v
toFunc (SoftClipRel p s) = softClipRel p s
toFunc (SoftClipAbs v s) = softClipAbs v s
toFunc (CompRel p s) = compRel p s
toFunc (CompAvg s) = compAvg s
toFunc (CompAbs v s) = compAbs v s
toFunc (Tremolo s d t) = tremolo s d t False
toFunc (Panning s d t) = tremolo s d t True
toFunc (Delay d f p) = delay d f p False
toFunc (Echo d f p) = delay d f p True
-------------------


optimizarEfectos :: Efecto -> Efecto -> Maybe Efecto

optimizarEfectos SetVolMax      SetVolMax       = Just SetVolMax
optimizarEfectos (SetVolRel v1) (SetVolRel v2)  = Just $ SetVolRel (v1*v2/100)
optimizarEfectos (NoiseGate x)  (NoiseGate y)   = Just $ NoiseGate (max x y)
optimizarEfectos (ClipAbs a)    (ClipAbs b)     = Just $ ClipAbs (min a b)
optimizarEfectos (SoftClipAbs _ 100) e = Just e
optimizarEfectos e (SoftClipAbs _ 100) = Just e
optimizarEfectos (SoftClipRel _ 100) e = Just e
optimizarEfectos e (SoftClipRel _ 100) = Just e
optimizarEfectos (SoftClipAbs v1 s1) (SoftClipAbs v2 s2) = if v1==v2 then Just $ SoftClipAbs v1 (s1*s2) else Nothing
optimizarEfectos (CompRel a1 s1)     (CompRel a2 s2)     = if a1==a2 then Just $ CompRel a1 (s1*s2) else Nothing
optimizarEfectos (CompAvg s1)        (CompAvg s2)        = Just $ CompAvg (s1*s2)
optimizarEfectos (CompAbs v1 s1)     (CompAbs v2 s2)     = if v1==v2 then Just $ CompAbs v1 (s1*s2) else Nothing
optimizarEfectos (Tremolo s1 d1 t1)  (Tremolo s2 d2 t2)  = if s1==s2 then Just $ Tremolo s1 (d1*d2) (t1+t2) else Nothing
optimizarEfectos (Panning s1 d1 t1)  (Panning s2 d2 t2)  = if s1==s2 then Just $ Panning s1 (d1*d2) (t1+t2) else Nothing
optimizarEfectos _ _ = Nothing


optimizar :: [Efecto] -> [Efecto]
optimizar [] = []
optimizar [e] = case e of
                    SoftClipAbs _ 100 -> []
                    SoftClipRel _ 100 -> []
                    _ -> [e]
optimizar (e1:(e2:es)) = case optimizarEfectos e1 e2 of
                            Just e -> optimizar (e:es)
                            _      -> e1 : (optimizar (e2:es))


applyEff :: String -> String -> [Efecto] -> IO ()
applyEff i o es = do wf <- readWav i
                     putStrLn "Wav parseado."

                     let f = foldl (>=>) return (map toFunc (optimizar es))
                     
--                     newwf' <- tremolo2 500 1 0.5 True wf
--                     newwf <- tremolo2 500 1 0.5 True newwf' --setVolMax2 newwf' --clipAbs2 32000 wf-- getSamples wf $$ putSamples wf --debug line
--                     x <- getMaxVolume2 newwf'
--                     putStrLn $ "max vol "++(show x)++" bps "++ (show $ bitsPerSample $ fmtheader wf)
                     
                     res <- f wf
                     putStrLn "Efectos aplicados."
                     writeWav o res
                     putStrLn "Wav final escrito."
                     return ()


-- | Left-to-right Kleisli composition of monads.
kff :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
kff !f !g !x = f x >>= g
