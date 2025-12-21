module ReplTests where

import Data.ByteString (ByteString)
import Lexer.Util
import Parser.Core
import Parser.Grammar.Grammar
import Parser.Parser
import Parser.Position (initial)
import Semantic.Error
import Semantic.Typecheck
import Semantic.TypedAST

createAST :: ByteString -> AST
createAST s = case runParser ast $ initial $ lexer s of
  Left err -> error err
  Right (tree, _) -> tree

typeofAST :: AST -> Either TypeError TypedExpr
typeofAST = typecheckAST (emptyCtx $ TypeVar "Foo")

typeof :: ByteString -> Either TypeError TypedExpr
typeof = typeofAST . createAST
