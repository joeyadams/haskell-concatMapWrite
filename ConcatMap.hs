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
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as U

newtype WB = WB { runWB :: Ptr Word8 -> IO (Ptr Word8) }

instance Monoid WB where
    mempty = WB return
    {-# INLINE mempty #-}

    a `mappend` b = WB $ \p -> do
        !p' <- runWB a p
        runWB b p'
    {-# INLINE mappend #-}

wb :: Word8 -> WB
wb b = WB $ \p -> do
    poke p b
    return $! (p `plusPtr` 1)
{-# INLINE wb #-}

data GrowException = GrowException
    deriving (Show, Typeable)

instance Exception GrowException


concatMap' :: (Word8 -> WB) -> ByteString -> ByteString
concatMap' f input =
    unsafePerformIO $
    U.unsafeUseAsCStringLen input $ \(rbuf, rlen) ->
        let !rp0 = castPtr rbuf
            !re  = rp0 `plusPtr` max 0 rlen

            bufLoop bufsize =
                handle (\GrowException -> bufLoop (bufsize * 2)) $
                    allocaBytes bufsize $ \wp -> do
                        we <- readLoop rp0 wp
                        B.packCStringLen (castPtr wp, we `minusPtr` wp)
            {-# INLINE bufLoop #-}

            readLoop rp wp | rp >= re  = return wp
                           | otherwise = do
                b <- peek rp
                let !rp' = rp `plusPtr` 1
                wp' <- runWB (f b) wp
                readLoop rp' wp'
            {-# INLINE readLoop #-}

         in bufLoop (B.length input * 5)
{-# INLINE concatMap' #-}
