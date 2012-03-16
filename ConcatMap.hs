{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ConcatMap (
    WB,
    wb,
    concatMap'
) where

import Control.Exception
import Control.Monad
import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Typeable        (Typeable)
import Data.Word            (Word8)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as U

newtype WB = WB { runWB :: Ptr Word8 -> Ptr Word8 -> IO (Maybe (Ptr Word8)) }

instance Monoid WB where
    mempty = WB $ \_we wp -> return $ Just wp
    {-# INLINE mempty #-}

    a `mappend` b = WB $ \we wp -> do
        m <- runWB a we wp
        case m of
            Nothing  -> return Nothing
            Just wp' -> runWB b we wp'
    {-# INLINE mappend #-}

wb :: Word8 -> WB
wb b = WB $ \we wp ->
    if wp < we
        then do
            poke wp b
            let !wp' = wp `plusPtr` 1
            return $ Just wp'
        else
            return Nothing
{-# INLINE wb #-}

type Convert = Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> IO (Maybe (Ptr Word8))

concatMapBuf :: (Word8 -> WB) -> Convert
concatMapBuf f = \re we rp0 wp0 ->
    let loop !rp !wp | rp >= re  = return $ Just wp
                     | otherwise = do
            b <- peek rp
            let !rp1 = rp `plusPtr` 1
            m <- runWB (f b) we wp
            case m of
                Nothing  -> return Nothing
                Just wp' -> loop rp1 wp'
     in loop rp0 wp0
{-# INLINE concatMapBuf #-}

concatMap' :: (Word8 -> WB) -> ByteString -> ByteString
concatMap' = runConvert . concatMapBuf
{-# INLINE concatMap' #-}

runConvert :: Convert -> ByteString -> ByteString
runConvert conv input =
    unsafePerformIO $
    U.unsafeUseAsCStringLen input $ \(rbuf, rlen) ->
        let !rp0 = castPtr rbuf
            !re  = rp0 `plusPtr` max 0 rlen

            bufLoop bufsize =
                join $ allocaBytes bufsize $ \wp -> do
                    m <- conv re (wp `plusPtr` bufsize) rp0 wp
                    case m of
                        Nothing  -> return $ do
                            hPutStrLn stderr "Realloc"
                            bufLoop (bufsize * 2)
                        Just wp' -> return $ B.packCStringLen (castPtr wp, wp' `minusPtr` wp)

         in bufLoop (B.length input * 2)
{-# INLINE runConvert #-}
