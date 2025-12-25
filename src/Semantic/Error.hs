module Semantic.Error where

import Data.ByteString (ByteString)

import Semantic.TypedAST

data TypeError
  = TypeMismatch TypedExpr TypedExpr
  | SubtypingLoop
  | ExprHasNoType
  | ExpectedTypeInExprButGot Type TypedExpr
  | IsNotSubtypeOf TypedExpr TypedExpr
  | UndeclaredIdentifier ByteString
  deriving Show
