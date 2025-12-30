module Lexer.Lexer where

import Data.ByteString (ByteString)

import Data.BSUtil (bytesToString)
import Lexer.Error
import Parser.Position

data LexemeTag
  = BOOL_CONST -- Bool
  | INT_CONST -- Int
  | STR_CONST -- String
  | OBJECTID -- identifier, starts with a lowercase letter
  | TYPEID -- identifier, starts with an uppercase letter
  | SEMICOLON -- ;
  | COLON -- :
  | DARROW -- =>
  | LBRACE -- {
  | RBRACE -- }
  | LPAREN -- (
  | RPAREN -- )
  | AT -- @
  | COMMA -- ,
  | DOT -- .
  | ADD -- +
  | SUB -- -
  | MUL -- \* (escaped bc my formatter removes it for some reason)
  | DIV -- /
  | TILDA -- ~
  | EQ -- =
  | LE -- <=
  | LT -- <
  | ASSIGN -- <-
  | IF -- if
  | THEN -- then
  | ELSE -- else
  | FI -- fi
  | CASE -- case
  | ESAC -- esac
  | LOOP -- loop
  | POOL -- pool
  | WHILE -- while
  | CLASS -- class
  | IN -- in
  | INHERITS -- inherits
  | ISVOID -- isvoid (tf is this?)
  | LET -- let
  | NEW -- new
  | OF -- of
  | NOT -- not
  | COMMENT -- makes parsing easier
  | EOF
  | ERR Error
  deriving (Show, Eq, Ord)

data Lexeme = Lexeme LexemeTag (Maybe ByteString)

instance Show Lexeme where
  show (Lexeme lexTag lexValue) =
    show lexTag
      ++ case lexValue of
        Nothing -> ""
        Just value -> " " ++ bytesToString value

mkLexComment :: Lexeme
mkLexComment = Lexeme COMMENT Nothing

mkLexError :: Error -> Lexeme
mkLexError err = Lexeme (ERR err) Nothing

mkLexStr :: ByteString -> Lexeme
mkLexStr s = Lexeme STR_CONST (Just s)

data LexInfo = LexInfo Lexeme SourcePos
  deriving Show

type Lexemes = [LexInfo]

extractTag :: LexInfo -> LexemeTag
extractTag (LexInfo (Lexeme tag _) _) = tag
