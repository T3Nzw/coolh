module Lexer.Parser.Combinators where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.BSUtil (char8, isAsciiLower8, isAsciiUpper8, isDigit8, toLower8)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Parser.Core

-- Parser combinators that work over ByteString.

-- Whitespace combinators
interval :: (Monoid e) => Parser e ByteString ()
interval = void $ sat (== char8 ' ')

tabulation :: (Monoid e) => Parser e ByteString ()
tabulation = void $ sat (== char8 '\t')

newline :: (Monoid e) => Parser e ByteString ()
newline = void $ sat (== char8 '\n')

formfeed :: (Monoid e) => Parser e ByteString ()
formfeed = void $ sat (== char8 '\f')

carriageret :: (Monoid e) => Parser e ByteString ()
carriageret = void $ sat (== char8 '\r')

vtabulation :: (Monoid e) => Parser e ByteString ()
vtabulation = void $ sat (== char8 '\v')

whitespace :: (Monoid e) => Parser e ByteString ()
whitespace = interval <|> tabulation <|> newline <|> formfeed <|> carriageret <|> vtabulation

whitespaces :: (Monoid e) => Parser e ByteString ()
whitespaces = void $ many whitespace

whitespaces1 :: (Monoid e) => Parser e ByteString ()
whitespaces1 = void $ many1 whitespace

token :: (Monoid e) => Parser e ByteString a -> Parser e ByteString a
token p = whitespaces *> p <* whitespaces

-- Alphabetical characters

lower :: (Monoid e) => Parser e ByteString Word8
lower = sat isAsciiLower8

upper :: (Monoid e) => Parser e ByteString Word8
upper = sat isAsciiUpper8

alpha :: (Monoid e) => Parser e ByteString Word8
alpha = lower <|> upper

-- Numerical characters

digit :: (Monoid e) => Parser e ByteString Word8
digit = sat isDigit8

-- Alphanumerical characters

alphanum :: (Monoid e) => Parser e ByteString Word8
alphanum = alpha <|> digit

-- Miscellaneous

underscore :: (Monoid e) => Parser e ByteString Word8
underscore = sat (== char8 '_')

dash :: (Monoid e) => Parser e ByteString Word8
dash = sat (== char8 '-')

quote :: (Monoid e) => Parser e ByteString Word8
quote = sat (== char8 '"')

null :: (Monoid e) => Parser e ByteString Word8
null = sat (== char8 '\0')

slash :: (Monoid e) => Parser e ByteString Word8
slash = sat (== char8 '\\')

-- TODO: escape sequences?

-- case-insensitive
charCI :: (Monoid e) => Word8 -> Parser e ByteString Word8
charCI x = sat (\y -> toLower8 x == toLower8 y)

stringCI :: (Monoid e) => ByteString -> Parser e ByteString ByteString
stringCI = Parser.Core.traverse charCI
