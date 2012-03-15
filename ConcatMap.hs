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

newtype WB = WB { runWB :: Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8) }

instance Monoid WB where
    mempty = WB $ \p _ -> return p
    {-# INLINE mempty #-}

    a `mappend` b = WB $ \p e -> do
        p' <- runWB a p e
        runWB b p' e
    {-# INLINE mappend #-}

wb :: Word8 -> WB
wb b = WB $ \p e ->
    if p < e
        then do
            poke p b
            return $! (p `plusPtr` 1)
        else
            throwIO GrowException
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
                        we <- readLoop rp0 wp (wp `plusPtr` bufsize)
                        B.packCStringLen (castPtr wp, we `minusPtr` wp)

            readLoop rp wp we | rp >= re  = return wp
                              | otherwise = do
                b <- peek rp
                let !rp' = rp `plusPtr` 1
                wp' <- runWB (f b) wp we
                readLoop rp' wp' we

         in bufLoop (B.length input * 2)
{-# INLINE concatMap' #-}
