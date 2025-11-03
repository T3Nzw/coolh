module Data.BSUtil
  ( isAsciiUpper8,
    isAsciiLower8,
    isDigit8,
    char8,
    word8,
    bytesToString,
    toLower8,
    controlToHex,
    surround8,
  )
where

import qualified Data.ByteString as B
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Functor.Contravariant (Contravariant (contramap), Op (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Numeric (showHex)

-- ByteString utilities inspired by Data.Char :)

char8 :: Char -> Word8
char8 = fromIntegral . fromEnum

word8 :: Word8 -> Char
word8 = chr . fromIntegral

lift8 :: (Char -> r) -> (Word8 -> r)
lift8 f = getOp $ contramap (chr . fromIntegral) (Op f)

isAsciiLower8 :: Word8 -> Bool
isAsciiLower8 = lift8 isAsciiLower

isAsciiUpper8 :: Word8 -> Bool
isAsciiUpper8 = lift8 isAsciiUpper

isDigit8 :: Word8 -> Bool
isDigit8 = lift8 isDigit

bytesToString :: B.ByteString -> String
bytesToString = T.unpack . TE.decodeUtf8

toLower8 :: Word8 -> Word8
toLower8 = char8 . toLower . word8

-- mimic antlr4's control sequence (+ other stuff) output
controlToHex :: Word8 -> String
controlToHex c
  | c == char8 '\\' = "\\\\" -- this is making me severely upset
  | c == char8 '\n' = "\n"
  | c >= 32 && c < 127 = pure . word8 $ c
  | otherwise = "<0x" ++ pad (showHex c "") ++ ">"
  where
    pad [x] = ['0', x]
    pad xs = xs

-- beautiful (if beautiful meant unreadble)
surround8 :: B.ByteString -> B.ByteString -> B.ByteString
surround8 x s = B.append x . B.append s $ x
