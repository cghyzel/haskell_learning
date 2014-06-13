{-| ByteStrings |-}

-- Like lists, but each element is one byte 

-- Strict or lazy

-- Strict

-- Data.ByteString (No laziness)
-- pros: less overhead because no thunks (promises)
-- cons: fill memory faster because evaluated all at once

-- Data.ByteString.Lazy (Lazy obv.)
-- Lazy bytestrings stored in chunks (64Kb)
-- Documentation : http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring/Data-ByteString-Lazy.html

-- pack :: [Word8] -> ByteString
-- think of it as taking a lazy list and making it less lazy (64kb)

-- unpack - inverse of pack

-- fromChunks [Strict.ByteString] -> Lazy.ByteString
-- toChunks Lazy.ByteString -> [Strict.ByteString]

-- B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
-- Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))


-- Chunks are 64 kb pieces of data

-- bytestring version of : is cons

-- B.cons 85 $ B.pack [80,81,82,84]  
-- Chunk "U" (Chunk "PQRT" Empty)
-- lazy though
-- cons' is the strict version
-- better for a lot of data
-- B.cons' 85 $ B.pack [80,81,82,84]  

-- foldr B.cons B.empty [50..60]  
-- Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<" Empty))))))))))

-- foldr B.cons' B.empty [50..60]  
-- Chunk "23456789:;<" Empty  


-- other operations analogous to Data.List
-- including but not limited to:  head, tail, init, null, length, map, reverse, foldl, foldr, concat, takeWhile, filter

-- readFile :: FilePath -> IO String
-- B.readFile :: FilePath -> IO ByteString

