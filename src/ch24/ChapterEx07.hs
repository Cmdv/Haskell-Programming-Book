module ChapterEx07 where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Word
import Text.Trifecta


data IPAddress6 =
  IPAddress6 Word64
             Word64
  deriving (Eq, Ord, Show)

type Base16 = Char

type Byte = [Base16]

byteToInt :: Byte -> Int
byteToInt []     = 0
byteToInt (h:hs) = digitToInt h * 16 ^ length hs + byteToInt hs

blockToInteger :: [Byte] -> Integer
blockToInteger [] = 0
blockToInteger (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToInteger bs

blockToW64 :: [Byte] -> Word64
blockToW64 [] = 0
blockToW64 (b:bs) =
  (fromIntegral $ byteToInt b) * (16 ^ 4) ^ (length bs) + blockToW64 bs

splitBlocks :: [Byte] -> ([Byte], [Byte])
splitBlocks = splitAt 4

blocksToIP6 :: ([Byte], [Byte]) -> IPAddress6
blocksToIP6 (b1, b2) = IPAddress6 (blockToW64 b1) (blockToW64 b2)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  bytes <- many $ try ((many hexDigit) <* char ':') <|> ((some hexDigit) <* eof)
  return $ blocksToIP6 $ splitBlocks $ mkFull bytes

mkFull :: [Byte] -> [Byte]
mkFull xs
  | length xs == 8 = xs
  | otherwise = go xs []
  where
    go (y:ys) acc
      | length y == 0 = mkFull (acc ++ (y : "" : ys))
      | otherwise = go ys (acc ++ [y])

ipEx1 :: String
ipEx1 = "0:0:0:0:0:ffff:ac10:fe01"

ipEx2 :: String
ipEx2 = "0:0:0:0:0:ffff:cc78:f"

ipEx3 :: String
ipEx3 = "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"

ipEx3' :: String
ipEx3' = "FE80::0202:B3FF:FE1E:8329"

ipEx4 :: String
ipEx4 = "2001:DB8::8:800:200C:417A"
