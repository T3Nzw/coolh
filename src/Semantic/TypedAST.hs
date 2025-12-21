{-# LANGUAGE RankNTypes #-}

-- you know shit is starting to get real
-- once you start needing a bunch of extensions

module Semantic.TypedAST where

import Data.BSUtil (bytesToString, string8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty, toList)

data Void -- lmao

data Self

data Type ty where
  VoidType :: Type Void
  SelfType :: Type Self
  NumberType :: Type Int
  BooleanType :: Type Bool
  StringType :: Type B.ByteString
  TypeVar :: B.ByteString -> Type B.ByteString

-- need an existential wrapper
data WrappedType where
  WrappedType :: Type ty -> WrappedType

instance Show (Type ty) where
  show VoidType = "Void"
  show SelfType = "Self"
  show NumberType = "Int"
  show BooleanType = "Bool"
  show StringType = "String"
  show (TypeVar var) = bytesToString var

-- "final" types: after inference, lub, etc
data TypedAST ty where
  TNumber :: Int -> TypedAST Int
  TBoolean :: Bool -> TypedAST Bool
  TStr :: B.ByteString -> TypedAST B.ByteString
  TId :: B.ByteString -> TypedAST B.ByteString
  TAssign :: TypedExpr -> TypedAST ty -> TypedAST ty
  TAdd :: TypedAST Int -> TypedAST Int -> TypedAST Int
  TSub :: TypedAST Int -> TypedAST Int -> TypedAST Int
  TMul :: TypedAST Int -> TypedAST Int -> TypedAST Int
  TDiv :: TypedAST Int -> TypedAST Int -> TypedAST Int
  TTilda :: TypedAST Int -> TypedAST Int
  TLt :: TypedAST Int -> TypedAST Int -> TypedAST Bool
  TLeq :: TypedAST Int -> TypedAST Int -> TypedAST Bool
  TEq :: TypedAST ty -> TypedAST ty -> TypedAST Bool
  TNot :: TypedAST Bool -> TypedAST Bool
  TIsVoid :: TypedAST ty -> TypedAST Bool
  TNew :: B.ByteString -> TypedAST ty
  TStatement :: [TypedExpr] -> TypedExpr -> TypedAST ty
  TIfThenElse :: TypedAST Bool -> TypedExpr -> TypedExpr -> TypedAST ty
  TWhileLoop :: TypedAST Bool -> TypedExpr -> TypedAST Void
  TCaseOf :: TypedAST ty0 -> NonEmpty (TypedAST ty) -> TypedAST ty
  TLetNoInit :: (B.ByteString, Type ty0) -> TypedAST ty1 -> TypedAST ty1
  TLetInit :: (B.ByteString, Type t0, TypedAST ty0) -> TypedAST ty1 -> TypedAST ty1

instance Show (TypedAST ty) where
  show (TNumber n) = show n
  show (TBoolean b) = show b
  show (TStr s) = show s
  show (TId x) = bytesToString x
  show (TAdd a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (TSub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (TMul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (TDiv a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (TTilda a) = "~" ++ show a
  show (TLt a b) = "(" ++ show a ++ " < " ++ show b ++ ")"
  show (TLeq a b) = "(" ++ show a ++ " <= " ++ show b ++ ")"
  show (TEq a b) = "(" ++ show a ++ " = " ++ show b ++ ")"
  show (TNot a) = "not " ++ show a
  show (TIsVoid a) = "isvoid " ++ show a
  show (TNew a) = "new " ++ show a
  show (TStatement stmts lastExpr) =
    "{ " ++ unwords (map show stmts) ++ "; " ++ show lastExpr ++ " }"
  show (TIfThenElse c t e) =
    "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e ++ " fi"
  show (TWhileLoop c b) =
    "while " ++ show c ++ " loop " ++ show b ++ " pool"
  show (TCaseOf e cases) =
    "case " ++ show e ++ " of " ++ show (toList cases) ++ " esac"
  show (TLetNoInit (iden, ty) body) =
    "let " ++ bytesToString iden ++ ":" ++ show ty ++ " in " ++ show body
  show (TLetInit (iden, ty, expr) body) =
    "let "
      ++ bytesToString iden
      ++ ":"
      ++ show ty
      ++ " <- "
      ++ show expr
      ++ " in "
      ++ show body

-- TODO: let, new, static and dynamic dispatch, noexpr, caseof

data TypedExpr where
  TypedExpr :: Type ty -> TypedAST ty -> TypedExpr

instance Show TypedExpr where
  show (TypedExpr ty ast) = "(" ++ show ast ++ " :: " ++ show ty ++ ")"

getType :: TypedExpr -> ByteString
getType (TypedExpr ty _) = string8 $ show ty

toType :: ByteString -> WrappedType
toType "Void" = WrappedType VoidType
toType "Self" = WrappedType SelfType
toType "Bool" = WrappedType BooleanType
toType "Int" = WrappedType NumberType
toType "String" = WrappedType StringType
toType typename = WrappedType $ TypeVar typename

-- WHAT THE FUCK
mapTE :: (forall ty. Type ty -> Type ty) -> (forall ty. TypedAST ty -> TypedAST ty) -> TypedExpr -> TypedExpr
mapTE ft fa (TypedExpr ty ast) = TypedExpr (ft ty) (fa ast)

data TypedFeature ty
