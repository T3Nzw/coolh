module Lexer.Util where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (^.))
import Control.Monad.State
import Data.BSUtil (bytesToString)
import Data.ByteString
import Lexer.Error
import Lexer.Lexer
import Lexer.Parser.Parser
import Parser.Core
import Parser.Position (lineNumber)
import qualified Parser.Position as Pos

type LexState = State Lexemes (Maybe (Pos.State ByteString))

lexer' :: Pos.State ByteString -> LexState
lexer' st = do
  -- NOTE: i could probably get rid of eof here
  let res = runParser (lexerP <|> Lexer.Parser.Parser.eof) st
  case res of
    Left _ -> error "lexer failed"
    Right (value@(Lexeme (ERR StringContainsUnescapedNewLine) _), st2) -> do
      let linfo = LexInfo value ((st2 ^. Pos.pos) & Pos.lineNumber %~ subtract 1)
      modify (linfo :)
      lexer' st2
    Right (value@(Lexeme tag _), st2) -> do
      let linfo = LexInfo value (st2 ^. Pos.pos)
      modify (linfo :)
      if tag == EOF
        then pure Nothing
        else lexer' st2

lexer :: ByteString -> Lexemes
lexer s = Prelude.reverse $ execState (lexer' $ Pos.initial s) []

lexfmt :: Lexemes -> String
lexfmt lexemes =
  Prelude.concatMap
    ( \(LexInfo lexeme pos) ->
        "#" ++ show (pos ^. lineNumber) ++ " " ++ lexafmt lexeme ++ "\n"
    )
    (Prelude.filter (\(LexInfo (Lexeme tag _) _) -> tag /= EOF && tag /= COMMENT) lexemes)

lexafmt :: Lexeme -> String
lexafmt (Lexeme tag Nothing) = lextagfmt tag
lexafmt (Lexeme tag@STR_CONST (Just value)) = lextagfmt tag ++ " " ++ show (bytesToString value)
lexafmt (Lexeme tag (Just value)) = lextagfmt tag ++ " " ++ bytesToString value

-- can't believe i actually hardcoded these
lextagfmt :: LexemeTag -> String
lextagfmt SEMICOLON = "';'"
lextagfmt COLON = "':'"
lextagfmt LBRACE = "'{'"
lextagfmt RBRACE = "'}'"
lextagfmt LPAREN = "'('"
lextagfmt RPAREN = "')'"
lextagfmt AT = "'@'"
lextagfmt COMMA = "','"
lextagfmt DOT = "'.'"
lextagfmt ADD = "'+'"
lextagfmt SUB = "'-'"
lextagfmt MUL = "'*'"
lextagfmt DIV = "'/'"
lextagfmt Lexer.Lexer.EQ = "'='"
lextagfmt TILDA = "'~'"
lextagfmt Lexer.Lexer.LT = "'<'"
lextagfmt (ERR err) = show err
lextagfmt tag = show tag
