module Semantic.Typecheck where

import Control.Lens (makeLenses, over)
import Control.Monad.State
import Data.ByteString.Char8
import Data.List (intersect)
import Data.Maybe (fromJust)

import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Parser.Parser
import Semantic.Error
import Semantic.TypedAST

type ObjectEnv = M.Map Objectid Type

type MethodEnv = M.Map Feature Type

type ClassEnv = M.Map Type (Maybe Type)

type ClassName = Type

data Context
  = Context
  { _objectEnv :: ObjectEnv
  , _methodEnv :: MethodEnv
  , _classEnv :: ClassEnv
  , _className :: ClassName
  }
  deriving Show

makeLenses ''Context

third :: (a, b, c) -> c
third (_, _, x) = x

builinClasses :: ClassEnv
builinClasses =
  M.fromList
    [ (ObjectType, Nothing)
    , (IOType, Just ObjectType)
    , (NumberType, Just ObjectType)
    , (BooleanType, Just ObjectType)
    , (StringType, Just ObjectType)
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

expect
  :: Type
  -> (TypedExpr -> TypedExpr -> TypedAST)
  -> Type
  -> Type
  -> TypedExpr
  -> TypedExpr
  -> Either TypeError TypedExpr
expect ret ctor ty1 ty2 expr1 expr2
  | ty1 == getType expr1 && ty2 == getType expr2 =
      Right $ TypedExpr ret $ ctor expr1 expr2
  | otherwise = Left $ TypeMismatch expr1 expr2

liftIntermediate
  :: (Monad m, MonadTrans t) => (a -> a -> m a) -> t m a -> t m a -> t m a
liftIntermediate f st1 st2 = do
  l <- st1
  r <- st2
  lift $ f l r

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
      Just ty -> pure $ TypedExpr ty (TId x)
  typecheckAST' (Assign _ iden@(Objectid x) ast _) = do
    ctx <- get
    case M.lookup iden (_objectEnv ctx) of
      Nothing -> lift $ Left $ UndeclaredIdentifier x
      Just ty -> do
        ast' <- typecheckAST' ast
        let subty = getType ast'
        if isSubtypeOf (_classEnv ctx) subty ty
          then pure ast'
          else lift $ Left $ IsNotSubtypeOf ast' (TypedExpr ty $ TId x)
  typecheckAST' (Parenthesised ast _) = typecheckAST' ast
  typecheckAST' (Add _ lhs rhs _) =
    liftIntermediate
      (expect NumberType TAdd NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Sub _ lhs rhs _) =
    liftIntermediate
      (expect NumberType TSub NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Mul _ lhs rhs _) =
    liftIntermediate
      (expect NumberType TMul NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Div _ lhs rhs _) =
    liftIntermediate
      (expect NumberType TDiv NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Lt _ lhs rhs _) =
    liftIntermediate
      (expect BooleanType TLt NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Leq _ lhs rhs _) =
    liftIntermediate
      (expect BooleanType TLeq NumberType NumberType)
      (typecheckAST' lhs)
      (typecheckAST' rhs)
  typecheckAST' (Eq _ lhs rhs _) = do
    lhs' <- typecheckAST' lhs
    rhs' <- typecheckAST' rhs
    if eqType lhs' rhs'
      && getType lhs' `Prelude.elem` [NumberType, BooleanType, StringType]
      then pure $ TypedExpr BooleanType (TEq lhs' rhs')
      else lift $ Left $ TypeMismatch lhs' rhs'
  typecheckAST' (Tilda _ ast _) = do
    ast' <- typecheckAST' ast
    if getType ast' == NumberType
      then pure $ TypedExpr NumberType $ TTilda ast'
      else lift $ Left $ ExpectedTypeInExprButGot NumberType ast'
  typecheckAST' (Not _ ast _) = do
    ast' <- typecheckAST' ast
    if getType ast' == BooleanType
      then pure $ TypedExpr BooleanType $ TNot ast'
      else lift $ Left $ ExpectedTypeInExprButGot BooleanType ast'
  typecheckAST' (IsVoid _ ast _) = do
    expr <- typecheckAST' ast
    pure $ TypedExpr BooleanType $ TIsVoid expr
  typecheckAST' (New _ (Typeid ty) _) = pure $ TypedExpr (toType ty) (TNew ty)
  typecheckAST' (Statement _ ne _) = do
    let lst = NE.toList ne
    res <- mapM typecheckAST' lst
    let seq = Prelude.init res
    let seqLast = Prelude.last res
    pure $ TypedExpr (getType seqLast) $ TStatement seq seqLast
  typecheckAST' (IfThenElse _ b t f _) = do
    cond <- typecheckAST' b
    true <- typecheckAST' t
    false <- typecheckAST' f
    case getType cond of
      BooleanType -> do
        ctx <- get
        let tflub = lub (_classEnv ctx) [getType true, getType false]
        pure $ TypedExpr tflub $ TIfThenElse cond true false
      _ -> lift $ Left $ ExpectedTypeInExprButGot BooleanType cond
  typecheckAST' (WhileLoop _ c b _) = do
    cond <- typecheckAST' c
    body <- typecheckAST' b
    case getType cond of
      BooleanType -> pure $ TypedExpr VoidType (TWhileLoop cond body)
      _ -> lift $ Left $ ExpectedTypeInExprButGot BooleanType cond
  typecheckAST' (LetNoInit _ (obj@(Objectid iden), Typeid tyid) body _) = do
    ctx <- get
    let ty = toType tyid
    let ty' = if ty == SelfType then _className ctx else ty
    modify (over objectEnv (M.insert obj ty'))
    body' <- typecheckAST' body
    -- rollback
    _ <- put ctx
    pure $ TypedExpr (getType body') $ TLetNoInit (iden, ty) body'
  typecheckAST' (LetInit _ (obj@(Objectid iden), Typeid tyid, expr) body _) = do
    ctx <- get
    let ty = toType tyid
    let ty' = if ty == SelfType then _className ctx else ty
    expr' <- typecheckAST' expr
    modify (over objectEnv (M.insert obj ty'))
    body' <- typecheckAST' body
    -- rollback
    _ <- put ctx
    if isSubtypeOf (_classEnv ctx) (getType expr') ty'
      then pure $ TypedExpr (getType body') $ TLetInit (iden, ty', expr') body'
      else undefined
  typecheckAST' (CaseOf _ ast pms _) = do
    expr <- typecheckAST' ast
    let pms' =
          Prelude.map (\(PMatch iden (Typeid ty) expr _) -> (iden, toType ty, expr))
            $ NE.toList pms
    ctx <- get
    mapM_ (\(iden, ty, _) -> modify (over objectEnv (M.insert iden ty))) pms'
    exprs <- mapM (typecheckAST' . third) pms'
    let exprs' = Prelude.zipWith (\(Objectid iden, ty, _) expr -> (iden, ty, expr)) pms' exprs
    let elub = lub (_classEnv ctx) $ Prelude.map getType exprs
    _ <- put ctx
    pure $ TypedExpr elub $ TCaseOf expr $ NE.fromList exprs'

typecheckFeature :: Context -> Feature -> Either TypeError TypedFeature
typecheckFeature ctx' feat = evalStateT (typecheckFeature' feat) ctx'
 where
  typecheckFeature' :: Feature -> StateT Context (Either TypeError) TypedFeature
  typecheckFeature' (AttrNoInit obj@(Objectid iden) (Typeid tyid) _) = do
    let ty = toType tyid
    -- TODO: idk?
    modify (over objectEnv (M.insert obj ty))
    pure $ TypedFeature ty (TAttrNoInit iden ty)
  typecheckFeature' (AttrInit obj@(Objectid iden) (Typeid tyid) expr _) = do
    let ty = toType tyid
    -- maybe not...
    modify (over objectEnv (M.insert obj ty))
    ctx <- get
    modify (over objectEnv (M.insert (Objectid "self") SelfType))
    ctx' <- get
    let expr' = typecheckAST ctx' expr
    _ <- put ctx
    case expr' of
      Left err -> lift $ Left err
      Right e@(TypedExpr ety _) ->
        if isSubtypeOf (_classEnv ctx') ety ty
          then pure $ TypedFeature ty $ TAttrInit iden ty e
          else lift $ Left $ IsNotSubtypeOf e (TypedExpr ty $ TId iden)
  typecheckFeature' (Method obj@(Objectid iden) args (Typeid tyid) body _) = undefined

-- TODO: static dispatch, dynamic dispatch, attributes, methods

typecheckClass :: Context -> Class -> Either TypeError TypedClass
typecheckClass = undefined

typecheckProgram :: Context -> Program -> Either TypeError TypedProgram
typecheckProgram = undefined

class Typeable a atyped | a -> atyped where
  typeof :: Context -> a -> Either TypeError atyped

instance Typeable AST TypedExpr where
  typeof = typecheckAST

instance Typeable Feature TypedFeature where
  typeof = typecheckFeature

instance Typeable Class TypedClass where
  typeof = undefined

instance Typeable Program TypedProgram where
  typeof = undefined
