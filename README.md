I'm trying to do reasonably fast character escaping in Haskell (that is, within a few factors of C).  Using [plain `ByteString` manipulation][1], my Haskell implementation is 125 times slower than my C implementation.  Then I tried to use [blaze-builder][2].  That was even slower.

I eventually managed to get it to about 1.5 times slower than the C version (very good!).  However, it uses a lot of ugly buffer manipulation, and the performance boost is very sensitive to what gets inlined and what doesn't.

The key function is this:

    concatMapWrite :: (Word8 -> Write) -> ByteString -> ByteString

It is like [concatMap from Data.ByteString][3].  However, instead of the callback returning a `ByteString`, it returns a [Write][4] object (defined in the [blaze-builder][5] package), which is used to write the data directly into a buffer.

Is there a better way to translate a sequence of bytes in Haskell than this?

 [1]: http://codereview.stackexchange.com/questions/9998/optimizing-bytestring-escaping

 [2]: http://hackage.haskell.org/package/blaze-builder

 [3]: http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString.html#v:concatMap

 [4]: http://hackage.haskell.org/packages/archive/blaze-builder/latest/doc/html/Blaze-ByteString-Builder.html#t:Write

 [5]: http://hackage.haskell.org/package/blaze-builder
