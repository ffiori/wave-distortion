module WavWrite (writeWav) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Data.Char
import Data.Bits
import Data.Binary.Put
import Data.Word
import System.Endian    -- fue necesario instalarlo con cabal install cpu
import System.IO
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import System.Directory (removeFile)

import WavTypes


class Monad m => Escritor m where
    merror :: String -> m ()
    putWord8' :: Word8 -> m ()
    putWord16le' :: Word16 -> m ()
    putWord24le' :: Word32 -> m ()
    putWord32le' :: Word32 -> m ()
    putLazyByteString' :: BS.ByteString -> m ()
    runPut' :: m () -> BS.ByteString
    
instance Escritor PutM where
    merror = error
    putLazyByteString' = putByteString
    putWord8' = putWord8
    putWord16le' = putWord16le
    putWord24le' = putWord24le
    putWord32le' = putWord32le
    runPut' = BL.toStrict . runPut


type MonadaEscritora = Put


writeWav :: FilePath -> WavFile -> IO ()
writeWav path wf = do handle <- openBinaryFile path WriteMode
                      BS.hPut handle $ runPut' ((buildWavHeader wf)::MonadaEscritora)
                      putWaves2 wf handle

buildWavHeader :: Escritor m => WavFile -> m ()
buildWavHeader wf = do putRIFF wf
                       putfmt wf
                       putdata wf

putRIFF :: Escritor m => WavFile -> m ()
putRIFF wf = let rc = riffheader wf
             in do putLazyByteString' . BS.pack $ map (fromIntegral.ord) $ chunkID rc
                   putWord32le' . fromIntegral $ chunkSize rc
                   putLazyByteString' . BS.pack $ map (fromIntegral.ord) $ format rc

putfmt :: Escritor m => WavFile -> m ()
putfmt wf = let fc = fmtheader wf
            in do putLazyByteString' . BS.pack $ map (fromIntegral.ord) $ subchunk1ID fc
                  putWord32le' . fromIntegral $ subchunk1Size fc
                  putWord16le' . fromIntegral $ audioFormat fc
                  putWord16le' . fromIntegral $ numChannels fc
                  putWord32le' . fromIntegral $ sampleRate fc
                  putWord32le' . fromIntegral $ byteRate fc
                  putWord16le' . fromIntegral $ blockAlign fc
                  putWord16le' . fromIntegral $ bitsPerSample fc
                  if audioFormat fc /= -2 then return ()
                  else let cbSize'    = case cbSize fc of
                                            Just x -> x
                                            Nothing -> error "Format header dañado."
                           validBitsPerSample' = case validBitsPerSample fc of
                                                    Just x -> x
                                                    Nothing -> error "Format header dañado."
                           chMask'    = case chMask fc of
                                            Just x -> x
                                            Nothing -> error "Format header dañado."
                           subFormat' = case subFormat fc of
                                            Just x -> x
                                            Nothing -> error "Format header dañado."
                           check'     = case check fc of
                                            Just x -> x
                                            Nothing -> error "Format header dañado."
                       in do putWord16le' . fromIntegral $ cbSize'
                             putWord16le' . fromIntegral $ validBitsPerSample'
                             putWord32le' . fromIntegral $ chMask'
                             putWord16le' . fromIntegral $ subFormat'
                             putLazyByteString' . BS.pack $ map (fromIntegral.ord) $ check'
                      

putdata :: Escritor m => WavFile -> m ()
putdata wf = let dc = dataheader wf
             in do putLazyByteString' . BS.pack $ map (fromIntegral.ord) $ chunk2ID dc
                   putWord32le' . fromIntegral $ chunk2Size dc

putWaves2 :: WavFile -> Handle -> IO ()
putWaves2 wf outHandle = let chs = chFiles $ dataheader wf
                             bps = bitsPerSample $ fmtheader wf
                             sampsz = div bps 8
                         in do
                              chHandles <- sequence [ openBinaryFile c ReadMode | c<-chs ]
                              putWaves2_ sampsz chHandles $$ putBlock2 outHandle
                              liftIO $ sequence $ map removeFile chs --una vez que escribí los canales en el archivo final, borro los temporales.
                              return ()

--SOURCE
--Obtiene los samples de tamaño sampsz bytes de cada canal.
putWaves2_ :: Int -> [Handle] -> Source IO [BS.ByteString]
putWaves2_ _ [] = error "No hay canales en putWaves"
putWaves2_ sampsz chHandles = 
    let leer h = do 
            eof <- liftIO $ hIsEOF h
            if eof
                then return Nothing
                else do str <- BS.hGet h sampsz
                        return $ Just str
    in do
        res <- liftIO $ sequence $ map leer chHandles
        let noNothing _ Nothing = False
            noNothing False _ = False
            noNothing _ _ = True
            check = foldl noNothing True res
        if check then do yield $ map (\x-> case x of Just xx -> xx) res
                         putWaves2_ sampsz chHandles
                 else do liftIO $ sequence $ map hClose chHandles
                         return ()

--SINK                                         
--escribe un sample de cada canal, o sea que conforma un bloque en el archivo.
putBlock2 :: Handle -> Sink [BS.ByteString] IO ()
putBlock2 handle = do
    mx <- await
    case mx of
        Nothing -> liftIO $ (hFlush>>hClose) handle
        Just samples -> do 
            liftIO $ sequence $ map (BS.hPut handle) samples
            putBlock2 handle
            


-- función que falta de la familia putWord
putWord24le :: Word32 -> Put
putWord24le xle = let xbe = toBE32 xle
                  in do putWord8 $ fromIntegral xbe
                        putWord8 $ fromIntegral (shiftR xbe 8)
                        putWord8 $ fromIntegral (shiftR xbe 16)
