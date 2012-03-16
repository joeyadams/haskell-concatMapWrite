{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ConcatMap (
    concatMap'
) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal
import Control.Monad
import Data.ByteString          (ByteString)
import Data.Word                (Word8)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe         (unsafePerformIO)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as U

-- | A Convert computation reads from an input buffer and writes to an output buffer.
--
--  * Returns 'Nothing' if the output buffer is too small.
--
--  * Returns @Just wlen@ if all input bytes were consumed, where @wlen@ is the
--    number of bytes written to the output buffer.
type Convert = Ptr Word8    -- ^ End of input buffer
            -> Ptr Word8    -- ^ End of output buffer
            -> Ptr Word8    -- ^ Beginning of input buffer
            -> Ptr Word8    -- ^ Beginning of output buffer
            -> IO (Maybe Int)

-- | A Convert computation runs a 'Write' on each byte of an input buffer,
--   writing the results to an output buffer.  It returns @Nothing@ if the
--   buffer is too small, or @Just wp'@ if there was enough space to convert
--   all input bytes, where @wp'@ is the end of the bytes written.
concatMapBuf :: (Word8 -> Write) -> Convert
concatMapBuf f = \re we rp0 wp0 ->
    let loop !rp !wp | rp >= re  = return $ Just (wp `minusPtr` wp0)
                     | otherwise = do
            b <- peek rp
            let !rp' = rp `plusPtr` 1
                w = f b
            if (we `minusPtr` wp) < getBound w
                then return Nothing
                else do
                    wp' <- runWrite w wp
                    loop rp' wp'
     in loop rp0 wp0
{-# INLINE concatMapBuf #-}

concatMap' :: (Word8 -> Write) -> ByteString -> ByteString
concatMap' = runConvert . concatMapBuf
-- concatMap' f = toByteString . fromWriteList f . B.unpack
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
                        Nothing   -> return $ bufLoop (bufsize * 2)
                        Just wlen -> return $ B.packCStringLen (castPtr wp, wlen)

         in bufLoop (B.length input * 2)
{-# INLINE runConvert #-}
