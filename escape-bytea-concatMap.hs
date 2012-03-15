import ConcatMap

import Data.Bits
import Data.ByteString          (ByteString)
import Data.ByteString.Char8    ()
import Data.ByteString.Internal (c2w)
import Data.Monoid

import qualified Data.ByteString.Lazy   as L

escapeCopyBytea :: ByteString -> ByteString
escapeCopyBytea = concatMap' f
    where
        f c | c == c2w '\\' = wb 92
                    `mappend` wb 92
                    `mappend` wb 92
                    `mappend` wb 92
        f c | c >= 32 && c <= 126 = wb c
        f c =             wb (c2w '\\')
                `mappend` wb (c2w '\\')
                `mappend` (wb $ c2w '0' + ((c `shiftR` 6) .&. 0x7))
                `mappend` (wb $ c2w '0' + ((c `shiftR` 3) .&. 0x7))
                `mappend` (wb $ c2w '0' + (c .&. 0x7))

mapChunks :: (ByteString -> ByteString) -> L.ByteString -> L.ByteString
mapChunks f = L.fromChunks . map f . L.toChunks

main :: IO ()
main = L.getContents >>= L.putStr . mapChunks escapeCopyBytea
