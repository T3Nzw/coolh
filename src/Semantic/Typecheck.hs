module Semantic.Typecheck where

import Control.Monad.State
import Data.BSUtil (string8)
import qualified Data.ByteString as B
import Data.ByteString.Char8
import Data.List (intersect)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Type.Equality ((:~:) (..))
import Parser.Parser
import Semantic.Error
import Semantic.TypedAST

eqType :: Type a -> Type b -> Maybe (a :~: b)
eqType VoidType VoidType = Just Refl
eqType NumberType NumberType = Just Refl
eqType BooleanType BooleanType = Just Refl
eqType StringType StringType = Just Refl
eqType (TypeVar var1) (TypeVar var2) = if var1 == var2 then Just Refl else Nothing
eqType _ _ = Nothing

-- talk about a suboptimal representation
type ObjectEnv = M.Map Objectid TypedExpr -- ?

type MethodEnv = M.Map Feature TypedExpr -- ?

type ClassEnv = M.Map ClassName (Maybe ClassName)

type ClassName = B.ByteString

data Context
  = Context
  { _objectEnv :: ObjectEnv,
    _methodEnv :: MethodEnv,
    _classEnv :: ClassEnv,
    _className :: ClassName
  }

builinClasses :: ClassEnv
builinClasses =
  M.fromList
    [ ("Object", Nothing),
      ("IO", Just "Object"),
      ("Int", Just "Object"),
      ("Bool", Just "Object"),
      ("String", Just "Object")
    ]

ancestors :: ClassEnv -> ClassName -> [ClassName]
ancestors cenv c =
  c : case M.lookup c cenv of
    Just (Just parent) -> ancestors cenv parent
    _ -> []

lub' :: ClassEnv -> [ClassName] -> [ClassName]
lub' _ [] = error "every two types in cool have a lub! :("
lub' cenv [c] = ancestors cenv c
lub' cenv (c : cs) = ancestors cenv c `intersect` lub' cenv cs

lub :: ClassEnv -> [ClassName] -> ClassName
lub cenv cs = Prelude.head $ lub' cenv cs

emptyCtx :: ClassName -> Context
emptyCtx = Context M.empty M.empty builinClasses

readBool :: B.ByteString -> Bool
readBool "false" = False
readBool "true" = True
readBool _ = error "this should never happen"

-- this was a mistake.
propTypedExpr ::
  Type ty ->
  Type ret ->
  (TypedAST ty -> TypedAST ty -> TypedAST ret) ->
  TypedExpr ->
  TypedExpr ->
  Either TypeError TypedExpr
propTypedExpr ty ret ctor (TypedExpr ty1 val1) (TypedExpr ty2 val2) =
  case (eqType ty ty1, eqType ty ty2) of
    (Just Refl, Just Refl) ->
      pure $ TypedExpr ret $ ctor val1 val2
    _ -> Left TypeMismatch

typecheck :: Context -> AST -> Either TypeError TypedExpr
typecheck ctx' expr = evalStateT (typecheck' expr) ctx'
  where
    typecheck' :: AST -> StateT Context (Either TypeError) TypedExpr
    typecheck' (Number _ (Objectid x) _) = pure $ TypedExpr NumberType (TNumber . fst . fromJust $ readInt x)
    typecheck' (Boolean _ (Objectid b) _) = pure $ TypedExpr BooleanType (TBoolean $ readBool b)
    typecheck' (Str _ (Objectid str) _) = pure $ TypedExpr StringType (TStr str)
    typecheck' (Id _ iden@(Objectid x) _) = do
      ctx <- get
      case M.lookup iden (_objectEnv ctx) of
        Nothing -> lift $ Left $ UndeclaredIdentifier x
        Just expr' -> pure expr'
    -- failed to think of a one-liner... i suck at working with transformers
    typecheck' (Add _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType NumberType TAdd lhs' rhs'
    typecheck' (Sub _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType NumberType TSub lhs' rhs'
    typecheck' (Mul _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType NumberType TMul lhs' rhs'
    typecheck' (Div _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType NumberType TDiv lhs' rhs'
    typecheck' (Lt _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType BooleanType TLt lhs' rhs'
    typecheck' (Leq _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType BooleanType TLeq lhs' rhs'
    typecheck' (Eq _ lhs rhs _) = do
      lhs' <- typecheck' lhs
      rhs' <- typecheck' rhs
      lift $ propTypedExpr NumberType BooleanType TEq lhs' rhs'
    typecheck' (Tilda _ ast _) = do
      TypedExpr ty ast' <- typecheck' ast
      case ty of
        NumberType -> pure $ TypedExpr NumberType (TTilda ast')
        _ -> lift $ Left TypeMismatch
    typecheck' (Not _ ast _) = do
      TypedExpr ty ast' <- typecheck' ast
      case ty of
        BooleanType -> pure $ TypedExpr BooleanType (TNot ast')
        _ -> lift $ Left TypeMismatch
    typecheck' (IsVoid _ ast _) = do
      TypedExpr ty ast' <- typecheck' ast
      pure $ TypedExpr BooleanType (TIsVoid ast')
    typecheck' (New _ (Typeid ty) _) = pure $ TypedExpr (TypeVar ty) (TNew ty)
    typecheck' (Statement _ ne _) = do
      let lst = NE.toList ne
      res <- mapM typecheck' lst
      let seq = Prelude.init res
      let seqLast = Prelude.last res
      case seqLast of
        TypedExpr ty _ -> pure $ TypedExpr ty (TStatement seq seqLast)
        _ -> error "this is unreachable BUT HOW TF DID THIS EVEN WORK? man haskell is magical"
    typecheck' (IfThenElse _ b t f _) = do
      TypedExpr condty cond <- typecheck' b
      true <- typecheck' t
      false <- typecheck' f
      case condty of
        BooleanType -> do
          ctx <- get
          let tfLub = lub (_classEnv ctx) [getType true, getType false]
          pure $ TypedExpr (TypeVar tfLub) (TIfThenElse cond true false)
        _ -> lift $ Left TypeMismatch
    typecheck' (WhileLoop _ c b _) = do
      TypedExpr condty cond <- typecheck' c
      body <- typecheck' b
      case condty of
        BooleanType -> pure $ TypedExpr VoidType (TWhileLoop cond body)
        _ -> lift $ Left TypeMismatch
