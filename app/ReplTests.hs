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

createFeature :: ByteString -> Feature
createFeature s = case runParser feature $ initial $ lexer s of
  Left err -> error err
  Right (feat, _) -> feat

typeAST :: ByteString -> Either TypeError TypedExpr
typeAST = typeof (emptyCtx $ TypeVar "Foo") . createAST

typeFeature :: ByteString -> Either TypeError TypedFeature
typeFeature = typeof (emptyCtx $ TypeVar "Foo") . createFeature
