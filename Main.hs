module Main where

import WavEdition (applyEff)
import Config (config)

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
         [arg1,arg2] -> applyEff arg1 arg2 config
         _ -> putStrLn $ "El programa se debe ejecutar como $ ./Main arg1 arg2. Donde arg1 es la ruta del archivo de entrada, y arg2 la del de salida."
