{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ConcatMapWrite (
    concatMapWrite
) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal
import Control.Monad
import Data.ByteString          (ByteString)
import Data.Word                (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe         (unsafePerformIO)

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe   as U

-- | Like 'B.concatMap', but have the function return a 'Write' containing the
-- generated output.  This can be used to write efficient escaping code.
concatMapWrite :: (Word8 -> Write) -> ByteString -> ByteString
concatMapWrite = runConvert . concatMapBuf

-- Naive implementation.  This is about 8 to 30 times slower (slower when the
-- function only returns a single byte more often).
-- concatMapWrite f = toByteString . fromWriteList f . B.unpack
{-# INLINE concatMapWrite #-}

-- | A Convert computation reads from an input buffer and writes to an output buffer.
--
-- Returns @(rp', wp')@, indicating the end of bytes read and written.  If
-- @rp'@ is less than the end of the input buffer, it means the output buffer
-- was too small, and needs to be expanded.
type Convert = Ptr Word8    -- ^ End of input buffer
            -> Ptr Word8    -- ^ End of output buffer
            -> Ptr Word8    -- ^ Beginning of input buffer
            -> Ptr Word8    -- ^ Beginning of output buffer
            -> IO (Ptr Word8, Ptr Word8)

-- | Run a Convert computation on a ByteString.  This allocates a buffer, runs
-- the computation, and if the buffer is too small, expands it and continues at
-- the position where it left off.
runConvert :: Convert -> ByteString -> ByteString
runConvert conv input =
    unsafePerformIO $
    U.unsafeUseAsCStringLen input $ \(rbuf, rlen) -> do
        -- rs: Start of read buffer
        -- re: End of read buffer
        let rs = castPtr rbuf
            re = rs `plusPtr` rlen

            -- rp:      Current position in read buffer
            -- wi:      Current index in write buffer
            --          (number of bytes written by all previous iterations)
            -- fbuf:    Foreign pointer of write buffer
            -- bufsize: Size of write buffer
            loop rp wi fbuf bufsize =
                join $ withForeignPtr fbuf $ \ws -> do
                    (rp', wp') <- conv re (ws `plusPtr` bufsize)
                                       rp (ws `plusPtr` wi)
                    let wi' = wp' `minusPtr` ws
                    if rp' >= re
                        then return $ return $ I.fromForeignPtr fbuf 0 wi'
                        else do
                            let bufsize' = bufsize * 2
                            fbuf' <- I.mallocByteString bufsize'
                            withForeignPtr fbuf' $ \ws' -> do
                                I.memcpy ws' ws (fromIntegral wi')
                                return $ loop rp' wi' fbuf' bufsize'

        let bufsize0 = B.length input * 2
        fbuf0 <- I.mallocByteString bufsize0
        loop rs 0 fbuf0 bufsize0
{-# INLINE runConvert #-}

-- | Run a 'Write' for each byte in the input buffer, writing it to the output buffer.
concatMapBuf :: (Word8 -> Write) -> Convert

-- The lambda here is important!  If we say concatMapBuf f re we rp0 wp0 = ...,
-- the program will be five times slower.
concatMapBuf f = \re we rp0 wp0 ->
    let loop !rp !wp | rp >= re  = return (rp, wp)
                     | otherwise = do
            b <- peek rp
            let !rp' = rp `plusPtr` 1
                w = f b
            if (we `minusPtr` wp) < getBound w
                then return (rp, wp)
                else do
                    wp' <- runWrite w wp
                    loop rp' wp'
     in loop rp0 wp0
{-# INLINE concatMapBuf #-}
