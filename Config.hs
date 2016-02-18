module Config where

import WavEdition

config :: [Efecto]
config = [

---------- Editar desde aquí...

           --NoiseGate 3
          --, SoftClipRel 15 5
          --, CompAvg 70
          --Tremolo 500 1 0.5
          --Panning 500 1 0.5
          --,Echo 30 9 90
          --Delay 1000 4 100
          SetVolMax
          
---------- ...hasta aquí.

         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
