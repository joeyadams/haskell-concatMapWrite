import ConcatMapWrite

import Blaze.ByteString.Builder
import Data.Bits
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Monoid
import Data.Word                (Word8)

import qualified Data.ByteString.Lazy   as L

escapeCopyBytea :: ByteString -> ByteString
escapeCopyBytea = concatMapWrite escape1
{-# INLINE escapeCopyBytea #-}

escape1 :: Word8 -> Write
escape1 c | c == c2w '\\'
          = writeWord8 (c2w '\\') `mappend`
            writeWord8 (c2w '\\') `mappend`
            writeWord8 (c2w '\\') `mappend`
            writeWord8 (c2w '\\')
          | c >= 32 && c <= 126
          = writeWord8 c
          | otherwise
          = writeWord8 (c2w '\\') `mappend`
            writeWord8 (c2w '\\') `mappend`
            (writeWord8 $ c2w '0' + ((c `shiftR` 6) .&. 0x7)) `mappend`
            (writeWord8 $ c2w '0' + ((c `shiftR` 3) .&. 0x7)) `mappend`
            (writeWord8 $ c2w '0' + (c .&. 0x7))

mapChunks :: (ByteString -> ByteString) -> L.ByteString -> L.ByteString
mapChunks f = L.fromChunks . map f . L.toChunks

main :: IO ()
main = L.getContents >>= L.putStr . mapChunks escapeCopyBytea
