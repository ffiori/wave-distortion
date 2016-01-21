module Test where

import WavEdition

config :: [Efecto]
config = [-- NoiseGate 2
         --, SoftClipRel 15 5
         --, CompAvg 70
         --, Panning 1000 1 0.5
         --,Echo 1500 7 50
         --, SetVolMax
         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
