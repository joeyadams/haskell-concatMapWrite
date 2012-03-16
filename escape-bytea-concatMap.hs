import ConcatMap

import Blaze.ByteString.Builder
import Data.Bits
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    ()
import Data.ByteString.Internal (c2w)
import Data.Monoid

import qualified Data.ByteString.Lazy   as L

escapeCopyBytea :: ByteString -> ByteString
escapeCopyBytea = concatMapWrite f
    where
        f c | c == c2w '\\' = writeWord8 92
                    `mappend` writeWord8 92
                    `mappend` writeWord8 92
                    `mappend` writeWord8 92
        f c | c >= 32 && c <= 126 = writeWord8 c
        f c =             writeWord8 (c2w '\\')
                `mappend` writeWord8 (c2w '\\')
                `mappend` (writeWord8 $ c2w '0' + ((c `shiftR` 6) .&. 0x7))
                `mappend` (writeWord8 $ c2w '0' + ((c `shiftR` 3) .&. 0x7))
                `mappend` (writeWord8 $ c2w '0' + (c .&. 0x7))
        {-# INLINE f #-}
{-# INLINE escapeCopyBytea #-}

mapChunks :: (ByteString -> ByteString) -> L.ByteString -> L.ByteString
mapChunks f = L.fromChunks . map f . L.toChunks

main :: IO ()
main = L.getContents >>= L.putStr . mapChunks escapeCopyBytea
