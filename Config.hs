module Config where

import WavEdition

config :: [Efecto]
config = [-- NoiseGate 20
         --, SoftClipRel 15 5
         --, CompAvg 70
         --Tremolo 500 1 0.5
          Panning 500 1 0.5
          ,Tremolo 510 1 0.5
          --,Panning 510 1 0.5
          --Delay 1000 1 100
         -- Echo 1000 2 70
          --SetVolRel 50
          --SetVolMax
          
         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
