module Lexer.Error (Error (..)) where

import Data.List (intercalate)

data Error
  = UnknownError
  | InputNotExhausted
  | InvalidSymbol String
  | EOFInComment
  | UnmatchedCommentEnd
  | UnterminatedStringAtEOF
  | StringConstantTooLong Int
  | StringContainsUnescapedNewLine
  | StringContainsNullCharacter
  | StringContainsEscapedNullCharacter
  | CompoundError [Error]
  deriving (Eq, Read) -- NOTE: this is stupid

instance Semigroup Error where
  err1 <> err2 = CompoundError [err1, err2]

instance Monoid Error where
  mempty = UnknownError
  mappend = (<>)

instance Show Error where
  show (CompoundError err) = intercalate "\n" $ map show err
  show err = "ERROR: " ++ showErr err

showErr :: Error -> String
showErr UnknownError = "Unknown error"
showErr InputNotExhausted = "Input not exhausted"
showErr (InvalidSymbol err) = "Invalid symbol \"" ++ err ++ "\""
showErr EOFInComment = "EOF in comment"
showErr UnmatchedCommentEnd = "Unmatched *)"
showErr UnterminatedStringAtEOF = "Unterminated string at EOF"
showErr (StringConstantTooLong n) = "String constant too long (" ++ show n ++ "bytes long)"
showErr StringContainsUnescapedNewLine = "String contains unescaped new line"
showErr StringContainsNullCharacter = "String contains null character"
showErr StringContainsEscapedNullCharacter = "String contains escaped null character"
showErr err@(CompoundError _) = show err
