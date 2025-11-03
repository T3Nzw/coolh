module Data.Stream where

import Data.BSUtil (char8)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Parser.Position (SourcePos (..), State (..))

-- A stream-like type class.
-- Useful for unifying different stream types
-- (String, ByteString, Text)
class Stream s where
  type Element s
  empty :: s
  cons :: Element s -> s -> s
  uncons :: s -> Maybe (Element s, s)

  -- this completely breaks my abstraction but otherwise types are ambiguous...
  computeOffset :: State s -> Element s -> State s

instance Stream [Char] where
  type Element [Char] = Char
  empty = []
  cons = (:)
  uncons [] = Nothing
  uncons (h : t) = Just (h, t)
  computeOffset (State inp (SourcePos abso ln col tw)) c =
    case c of
      '\n' -> State inp $ SourcePos (abso + 1) (ln + 1) 0 tw
      '\t' -> State inp $ SourcePos (abso + tw) ln (col + tw) tw
      _ -> State inp $ SourcePos (abso + 1) ln (col + 1) tw

instance Stream BS.ByteString where
  type Element BS.ByteString = Word8
  empty = BS.empty
  cons = BS.cons
  uncons = BS.uncons
  computeOffset (State inp (SourcePos abso ln col tw)) c
    | c == char8 '\n' = State inp $ SourcePos (abso + 1) (ln + 1) 0 tw
    | c == char8 '\t' = State inp $ SourcePos (abso + tw) ln (col + tw) tw
    | otherwise = State inp $ SourcePos (abso + 1) ln (col + 1) tw
