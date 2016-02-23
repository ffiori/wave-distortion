module Config where

import WavEdition

config :: [Efecto]
config = [

---------- Editar desde aquí...

          -- NoiseGate 1,
           --CompRel 40 5,
           --NoiseGate 2,
           --SetVolMax,
           --SoftClipRel 1 10,
          --, CompAvg 70
          Tremolo 500 1 0.5,
          --Panning 200 1 0.5,
          --Echo 100 10 20,
          --Echo 500 2 80,
          SetVolMax
          --SetVolRel 80
          
---------- ...hasta aquí.

         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
