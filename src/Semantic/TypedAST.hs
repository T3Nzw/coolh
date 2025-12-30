module Semantic.TypedAST where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty, toList)

import qualified Data.ByteString as B

import Data.BSUtil (bytesToString)

data Type where
  ObjectType :: Type
  IOType :: Type
  VoidType :: Type
  SelfType :: Type
  NumberType :: Type
  BooleanType :: Type
  StringType :: Type
  TypeVar :: B.ByteString -> Type
  deriving (Eq, Ord)

instance Show Type where
  show ObjectType = "Object"
  show IOType = "IO"
  show VoidType = "Void"
  show SelfType = "SELF_TYPE"
  show NumberType = "Int"
  show BooleanType = "Bool"
  show StringType = "String"
  show (TypeVar var) = bytesToString var

data TypedAST
  = TNumber Int
  | TBoolean Bool
  | TStr B.ByteString
  | TId B.ByteString
  | TAssign B.ByteString TypedExpr
  | TAdd TypedExpr TypedExpr
  | TSub TypedExpr TypedExpr
  | TMul TypedExpr TypedExpr
  | TDiv TypedExpr TypedExpr
  | TTilda TypedExpr
  | TLt TypedExpr TypedExpr
  | TLeq TypedExpr TypedExpr
  | TEq TypedExpr TypedExpr
  | TNot TypedExpr
  | TIsVoid TypedExpr
  | TNew B.ByteString
  | TStatement [TypedExpr] TypedExpr
  | TIfThenElse TypedExpr TypedExpr TypedExpr
  | TWhileLoop TypedExpr TypedExpr
  | TCaseOf TypedExpr (NonEmpty (B.ByteString, Type, TypedExpr))
  | TLetNoInit (B.ByteString, Type) TypedExpr
  | TLetInit (B.ByteString, Type, TypedExpr) TypedExpr
  | TStaticDispatch
  | TDynamidDispatch

instance Show TypedAST where
  show (TNumber n) = show n
  show (TBoolean b) = show b
  show (TStr s) = show s
  show (TId x) = bytesToString x
  show (TAssign x expr) = bytesToString x ++ " <- " ++ show expr
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
  show (TStaticDispatch) = error "not yet implemented"
  show (TDynamidDispatch) = error "not yet implemented"

data TypedExpr where
  TypedExpr :: Type -> TypedAST -> TypedExpr

instance Show TypedExpr where
  show (TypedExpr ty ast) = "(" ++ show ast ++ " :: " ++ show ty ++ ")"

toType :: B.ByteString -> Type
toType "Object" = ObjectType
toType "IO" = IOType
toType "Void" = VoidType
toType "SELF_TYPE" = SelfType
toType "Int" = NumberType
toType "Bool" = BooleanType
toType "String" = StringType
toType name = TypeVar name

getType :: TypedExpr -> Type
getType (TypedExpr ty _) = ty

eqType :: TypedExpr -> TypedExpr -> Bool
eqType (TypedExpr ty1 _) (TypedExpr ty2 _) = ty1 == ty2

data TypedFeat
  = TAttrNoInit ByteString Type
  | TAttrInit ByteString Type TypedExpr
  | TMethod ByteString [(ByteString, Type)] Type TypedExpr
  deriving Show

data TypedFeature where
  TypedFeature :: Type -> TypedFeat -> TypedFeature

deriving instance Show TypedFeature

data TypedClass where
  TypedClass :: Type -> [TypedFeature] -> TypedClass

deriving instance Show TypedClass

data TypedProgram where
  TypedProgram :: [TypedClass] -> TypedProgram

deriving instance Show TypedProgram
