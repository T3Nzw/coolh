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

createClass :: ByteString -> Class
createClass s = case runParser (class' "dummy file name") $ initial $ lexer s of
  Left err -> error err
  Right (c, _) -> c

createProgram :: ByteString -> Program
createProgram s = case runParser (program "dummy file name") $ initial $ lexer s of
  Left err -> error err
  Right (p, _) -> p

typeAST :: ByteString -> Either TypeError TypedExpr
typeAST = typeof (emptyCtx $ TypeVar "Foo") . createAST

typeFeature :: ByteString -> Either TypeError TypedFeature
typeFeature = typeof (emptyCtx $ TypeVar "Foo") . createFeature

typeClass :: ByteString -> Either TypeError TypedClass
typeClass = typeof (emptyCtx $ TypeVar "Foo") . createClass

typeProgram :: ByteString -> Either TypeError TypedProgram
typeProgram = typeof (emptyCtx $ TypeVar "Foo") . createProgram
