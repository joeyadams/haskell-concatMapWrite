{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ConcatMap (
    WB,
    wb,
    concatMap'
) where

import Control.Monad
import Data.ByteString          (ByteString)
import Data.Monoid
import Data.Word                (Word8)
import Data.Typeable            (Typeable)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe         (unsafePerformIO)

import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as U

data WB = WB { runWB    :: !(Ptr Word8 -> IO (Ptr Word8))
             , lengthWB :: !Int
             }
    deriving Typeable

instance Monoid WB where
    mempty = WB return 0
    {-# INLINE mempty #-}

    a `mappend` b = WB (runWB a >=> runWB b) (lengthWB a + lengthWB b)
    {-# INLINE mappend #-}

wb :: Word8 -> WB
wb b = WB { runWB = \wp -> do
                poke wp b
                return $! (wp `plusPtr` 1)
          , lengthWB = 1
          }
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
            let !rp' = rp `plusPtr` 1
                WB r n = f b
            if (we `minusPtr` wp) < n
                then return Nothing
                else do
                    wp' <- r wp
                    loop rp' wp'
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
                        Nothing  -> return $ bufLoop (bufsize * 2)
                        Just wp' -> return $ B.packCStringLen (castPtr wp, wp' `minusPtr` wp)

         in bufLoop (B.length input * 2)
{-# INLINE runConvert #-}
