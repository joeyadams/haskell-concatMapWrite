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

type Convert = Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> IO (Maybe (Ptr Word8))

concatMapBuf :: (Word8 -> Write) -> Convert
concatMapBuf f = \re we rp0 wp0 ->
    let loop !rp !wp | rp >= re  = return $ Just wp
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
-- concatMap' = runConvert . concatMapBuf
concatMap' f = toByteString . fromWriteList f . B.unpack
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
                        Nothing  -> return $ bufLoop (bufsize * 2)
                        Just wp' -> return $ B.packCStringLen (castPtr wp, wp' `minusPtr` wp)

         in bufLoop (B.length input * 2)
{-# INLINE runConvert #-}
