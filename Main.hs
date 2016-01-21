-- más rápido que Data.WAVE ! (testeado en lectura y escritura solamente)

--Compilar con $ ghc Main.hs -O2 -package ghc -rtsopts -threaded --make
--time ./Main g.wav out.wav +RTS -s -qg2 -N2

-- http://co-dan.github.io/ghc-docs/GHC.html
-- https://parenz.wordpress.com/2013/08/17/ghc-api-interpreted-compiled-and-package-modules/

module Main where

import WavEdition (applyEff)

import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import Control.Applicative

{-
import DynFlags
import GHC
import GHC.Paths           (libdir) -- fue necesario instalarlo con cabal install ghc-paths
--import GhcMonad            (liftIO) -- from ghc7.7 and up you can use the usual liftIO from Control.Monad.IO.Class
import Unsafe.Coerce

main :: IO ()
main = defaultErrorHandler defaultLogAction $ do
    args <- getArgs
    case args of
         [arg1,arg2] -> runGhc (Just libdir) $ do
                -- we have to call 'setSessionDynFlags' before doing everything else
                dflags <- getSessionDynFlags
                
                -- If we want to make GHC interpret our code on the fly, we ought to set those two flags,
                -- otherwise we wouldn't be able to use 'setContext' below
                setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                            , ghcLink   = LinkInMemory
                                            }
                setTargets =<< sequence [ guessTarget "Test.hs" Nothing ]
                load LoadAllTargets

                -- Bringing the module into the context
                --esto lo que hacía era Bring into scope the entire top-level envt of of this module, including the things imported into it.
                --m <- getModSummary $ mkModuleName "Test" 
                --setContext [ IIModule $ ms_mod m ]
                
                --IIDecl lo que hace es Bring the exports of a particular module (filtered by an import decl) into scope.
                setContext [ IIDecl $ simpleImportDecl (mkModuleName "Test") ]

                -- evaluating and running an action
                efs <- compileExpr "Test.config" -- devuelve m HValue, o sea que efs::HValue, por eso el unsafeCoerce.        
                liftIO $ applyEff arg1 arg2 (unsafeCoerce efs) -- liftIO :: MonadIO m => IO a -> m a

         _ -> putStrLn $ "El programa se debe ejecutar como $ ./Main arg1 arg2. Donde arg1 es la ruta del archivo de entrada, y arg2 la del de salida."

-}

import Test

main :: IO ()
main = do
    args <- getArgs
    case args of
         [arg1,arg2] -> applyEff arg1 arg2 config
         _ -> putStrLn $ "El programa se debe ejecutar como $ ./Main arg1 arg2. Donde arg1 es la ruta del archivo de entrada, y arg2 la del de salida."


{-
    Antes tenía un archivo Config.hs (como XMonad), pero para setear las configuraciones del efecto que quería aplicar
    tenía que recompilar. Así solamente con modificar el Test.hs los cambios se aplican cada vez que se ejecuta el programa.
-}
