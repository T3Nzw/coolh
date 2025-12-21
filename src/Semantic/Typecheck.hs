module Semantic.Typecheck where

import Control.Lens (makeLenses, over)
import Control.Monad.State
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

makeLenses ''Context

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

isSubtypeOf :: ClassEnv -> ClassName -> ClassName -> Bool
isSubtypeOf cenv c1 c2 = c2 `Prelude.elem` lub' cenv [c1, c2]

-- TODO: do some form of an occurs-check for subtyping.
-- this should probably happen when creating a new class
-- (hence inheriting) and should be dependent on the context

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

typecheckAST :: Context -> AST -> Either TypeError TypedExpr
typecheckAST ctx' expr = evalStateT (typecheckAST' expr) ctx'
  where
    typecheckAST' :: AST -> StateT Context (Either TypeError) TypedExpr
    typecheckAST' (Number _ (Objectid x) _) = pure $ TypedExpr NumberType (TNumber . fst . fromJust $ readInt x)
    typecheckAST' (Boolean _ (Objectid b) _) = pure $ TypedExpr BooleanType (TBoolean $ readBool b)
    typecheckAST' (Str _ (Objectid str) _) = pure $ TypedExpr StringType (TStr str)
    typecheckAST' (Id _ iden@(Objectid x) _) = do
      ctx <- get
      case M.lookup iden (_objectEnv ctx) of
        Nothing -> lift $ Left $ UndeclaredIdentifier x
        Just expr' -> pure expr'
    typecheckAST' (Assign _ iden@(Objectid x) ast _) = do
      ctx <- get
      case M.lookup iden (_objectEnv ctx) of
        Nothing -> lift $ Left $ UndeclaredIdentifier x
        Just texpr -> do
          ast' <- typecheckAST' ast
          let subty = getType ast'
          let ty = getType texpr
          if isSubtypeOf (_classEnv ctx) subty ty
            then pure ast'
            else lift $ Left $ IsNotSubtypeOf subty ty
    typecheckAST' (Parenthesised ast _) = typecheckAST' ast
    -- failed to think of a one-liner... i suck at working with transformers
    typecheckAST' (Add _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType NumberType TAdd lhs' rhs'
    typecheckAST' (Sub _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType NumberType TSub lhs' rhs'
    typecheckAST' (Mul _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType NumberType TMul lhs' rhs'
    typecheckAST' (Div _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType NumberType TDiv lhs' rhs'
    typecheckAST' (Lt _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType BooleanType TLt lhs' rhs'
    typecheckAST' (Leq _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType BooleanType TLeq lhs' rhs'
    typecheckAST' (Eq _ lhs rhs _) = do
      lhs' <- typecheckAST' lhs
      rhs' <- typecheckAST' rhs
      lift $ propTypedExpr NumberType BooleanType TEq lhs' rhs'
    typecheckAST' (Tilda _ ast _) = do
      TypedExpr ty ast' <- typecheckAST' ast
      case ty of
        NumberType -> pure $ TypedExpr NumberType (TTilda ast')
        _ -> lift $ Left TypeMismatch
    typecheckAST' (Not _ ast _) = do
      TypedExpr ty ast' <- typecheckAST' ast
      case ty of
        BooleanType -> pure $ TypedExpr BooleanType (TNot ast')
        _ -> lift $ Left TypeMismatch
    typecheckAST' (IsVoid _ ast _) = do
      TypedExpr _ ast' <- typecheckAST' ast
      pure $ TypedExpr BooleanType (TIsVoid ast')
    typecheckAST' (New _ (Typeid ty) _) = pure $ TypedExpr (TypeVar ty) (TNew ty)
    typecheckAST' (Statement _ ne _) = do
      let lst = NE.toList ne
      res <- mapM typecheckAST' lst
      let seq = Prelude.init res
      let seqLast = Prelude.last res
      case seqLast of
        TypedExpr ty _ -> pure $ TypedExpr ty (TStatement seq seqLast)
    typecheckAST' (IfThenElse _ b t f _) = do
      TypedExpr condty cond <- typecheckAST' b
      true <- typecheckAST' t
      false <- typecheckAST' f
      case condty of
        BooleanType -> do
          ctx <- get
          let tfLub = lub (_classEnv ctx) [getType true, getType false]
          pure $ TypedExpr (TypeVar tfLub) (TIfThenElse cond true false)
        _ -> lift $ Left TypeMismatch
    typecheckAST' (WhileLoop _ c b _) = do
      TypedExpr condty cond <- typecheckAST' c
      body <- typecheckAST' b
      case condty of
        BooleanType -> pure $ TypedExpr VoidType (TWhileLoop cond body)
        _ -> lift $ Left TypeMismatch
    typecheckAST' (LetNoInit _ (obj@(Objectid iden), Typeid ty) body _) = do
      ctx <- get
      let ty' = if ty == "SELF_TYPE" then _className ctx else ty
      -- modify env
      let typedBinding = TypedExpr (TypeVar ty') (TId iden)
      modify (over objectEnv (M.insert obj typedBinding))
      TypedExpr bty body' <- typecheckAST' body
      -- rollback
      _ <- put ctx
      pure $ TypedExpr bty (TLetNoInit (iden, TypeVar ty) body')
    typecheckAST' (LetInit _ (obj@(Objectid iden), Typeid ty, expr) body _) = do
      ctx <- get
      let ty' = if ty == "SELF_TYPE" then _className ctx else ty
      e@(TypedExpr ety expr') <- typecheckAST' expr
      let etyS = getType e
      -- modify env
      let typedBinding = TypedExpr (TypeVar ty') (TId iden)
      modify (over objectEnv (M.insert obj typedBinding))
      TypedExpr bty body' <- typecheckAST' body
      -- rollback
      _ <- put ctx
      if isSubtypeOf (_classEnv ctx) etyS ty'
        then pure $ TypedExpr bty (TLetInit (iden, TypeVar ty, e) body')
        else lift $ Left $ IsNotSubtypeOf etyS ty'

typecheckFeature :: Context -> Feature -> Either TypedExpr a
typecheckFeature = undefined

-- TODO: caseof, letinit, letnoinit, static dispatch, dynamic dispatch, attributes, methods

class Typeable a atyped | a -> atyped where
  typeof :: Context -> a -> Either TypeError atyped

instance Typeable AST TypedExpr where
  typeof = typecheckAST
