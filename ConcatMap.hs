{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ConcatMap (
    WB,
    wb,
    concatMap'
) where

import Control.Exception
import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Typeable        (Typeable)
import Data.Word            (Word8)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as U

newtype WB = WB { runWB :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8, Ptr Word8) }

instance Monoid WB where
    mempty = WB $ \_re rp wp -> return (rp, wp)
    {-# INLINE mempty #-}

    a `mappend` b = WB $ \re rp wp -> do
        (rp', wp') <- runWB a re rp wp
        runWB b re rp' wp'
    {-# INLINE mappend #-}

wb :: Word8 -> WB
wb b = WB $ \_re rp wp -> do
    poke wp b
    let !wp' = wp `plusPtr` 1
    return (rp, wp')
{-# INLINE wb #-}

data GrowException = GrowException
    deriving (Show, Typeable)

instance Exception GrowException

type Convert = Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> IO (Ptr Word8)

concatMapBuf :: (Word8 -> WB) -> Convert
concatMapBuf f re rp0 wp0 =
        loop rp0 wp0
    where
        loop !rp !wp | rp >= re  = return wp
                     | otherwise = do
            b <- peek rp
            let !rp1 = rp `plusPtr` 1
            (rp', wp') <- runWB (f b) re rp1 wp
            loop rp' wp'
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
                handle (\GrowException -> bufLoop (bufsize * 2)) $
                    allocaBytes bufsize $ \wp -> do
                        wp' <- conv re rp0 wp
                        B.packCStringLen (castPtr wp, wp' `minusPtr` wp)

         in bufLoop (B.length input * 5)
{-# INLINE runConvert #-}
