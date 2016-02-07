module Config where

import WavEdition

config :: [Efecto]
config = [

---------- Editar desde aquí...

           --NoiseGate 1
          --, SoftClipRel 15 5
          --, CompAvg 70
          --Tremolo 500 1 0.5
          --,Panning 500 1 0.5
          Delay 3000 4 100
          --Delay 1000 4 100
         -- SetVolMax
          
---------- ...hasta aquí.

         ]
         
-- Los efectos se aplican en el orden en que están escritos en la lista.
