module Semantic.Error where

import Data.ByteString (ByteString)
import Data.List

import Data.BSUtil (bytesToString)
import Semantic.TypedAST

data TypeError
  = TypeMismatch TypedExpr TypedExpr
  | SubtypingLoop [Type]
  | ExprHasNoType
  | ExpectedTypeInExprButGot Type TypedExpr
  | IsNotSubtypeOf TypedExpr TypedExpr
  | UndeclaredIdentifier ByteString
  | RedefinitionOfClass Type

instance Show TypeError where
  show (TypeMismatch e1 e2) =
    "detected type mismatch between expressions "
      ++ show e1
      ++ " and "
      ++ show e2
      ++ " while expecting them to be of the same type"
  show (SubtypingLoop types) =
    "detected subtyping loop in the following type hierarchy: "
      ++ intercalate " <- " (map show types)
  show ExprHasNoType = error "what do i even need this for?"
  show (ExpectedTypeInExprButGot expectedType e@(TypedExpr exprType _)) =
    "expected an expression of type "
      ++ show expectedType
      ++ " in "
      ++ show e
      ++ " but got type "
      ++ show exprType
      ++ " instead"
  show (IsNotSubtypeOf e1@(TypedExpr ty1 _) e2@(TypedExpr ty2 _)) =
    show ty1
      ++ " is not a subtype of "
      ++ show ty2
      ++ ": in expressions "
      ++ show e1
      ++ " and "
      ++ show e2
  show (UndeclaredIdentifier iden) = "undeclared identifier '" ++ bytesToString iden ++ "'"
  show (RedefinitionOfClass ty) = "redefinition of class " ++ show ty
