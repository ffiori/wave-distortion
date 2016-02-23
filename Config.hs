module Config where

import WavEdition

config :: [Efecto]
config = [

---------- Editar desde aquí...

          SetVolMax,
          --NoiseGate 1,
          --CompRel 40 5,
          --SoftClipRel 5 30,
          --CompAvg 10,
          --Tremolo 500 1 0.5,
          --Panning 200 1 0,
          Echo 100 10 20
          --SetVolMax
          --SetVolRel 80
          
---------- ...hasta aquí.

         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
{-
Efectos para usar:
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
-}
