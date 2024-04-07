{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module SPL.Typechecker where

import SPL.Parser.AST
import SPL.Parser.Parser (SourceSpan)
import Data.List

type instance FunDecl AnnotatedP = SourceSpan
type instance FunDeclT AnnotatedP = Type
type instance VarDecl AnnotatedP = SourceSpan
type instance VarDeclT AnnotatedP = Type

type instance ReturnStmt AnnotatedP = SourceSpan
type instance IfStmt AnnotatedP = SourceSpan
type instance WhileStmt AnnotatedP = SourceSpan
type instance ExprStmt AnnotatedP = SourceSpan
type instance VarStmt AnnotatedP = SourceSpan

type instance BinOpExpr AnnotatedP = SourceSpan
type instance UnaryOpExpr AnnotatedP = SourceSpan
type instance AssignExpr AnnotatedP = SourceSpan
type instance FunctionCallExpr AnnotatedP = SourceSpan
type instance VariableExpr AnnotatedP = SourceSpan
type instance LiteralExpr AnnotatedP = SourceSpan

type instance FunDecl TypecheckedP = SourceSpan
type instance VarDecl TypecheckedP = SourceSpan

type instance ReturnStmt TypecheckedP = SourceSpan
type instance IfStmt TypecheckedP = SourceSpan
type instance WhileStmt TypecheckedP = SourceSpan
type instance ExprStmt TypecheckedP = SourceSpan
type instance VarStmt TypecheckedP = SourceSpan

type instance BinOpExpr TypecheckedP = (SourceSpan, Type)
type instance UnaryOpExpr TypecheckedP = (SourceSpan, Type)
type instance AssignExpr TypecheckedP = (SourceSpan, Type)
type instance FunctionCallExpr TypecheckedP = (SourceSpan, Type)
type instance VariableExpr TypecheckedP = (SourceSpan, Type)
type instance LiteralExpr TypecheckedP = (SourceSpan, Type)

instance Convertable Stmt ParsedP AnnotatedP where
  convert (ReturnStmt pos expr) =
    ReturnStmt pos (convert <$> expr)
  convert (IfStmt pos condition consequent alternative) =
    IfStmt pos (convert condition) (convert <$> consequent) (map convert <$> alternative)
  convert (WhileStmt pos condition body) =
    WhileStmt pos (convert condition) (convert <$> body)
  convert (ExprStmt pos expr) =
    ExprStmt pos (convert expr)
  convert (VarStmt pos ty name expr) =
    VarStmt pos ty name (convert expr)

instance Convertable Expr ParsedP AnnotatedP where
  convert (BinOpExpr pos op left right) =
    BinOpExpr pos op (convert left) (convert right)
  convert (UnaryOpExpr pos op expr) =
    UnaryOpExpr pos op (convert expr)
  convert (AssignExpr pos x expr) =
    AssignExpr pos x (convert expr)
  convert (FunctionCallExpr pos name args) =
    FunctionCallExpr pos name (convert <$> args)
  convert (VariableExpr pos x) =
    VariableExpr pos x
  convert (LiteralExpr pos lit) =
    LiteralExpr pos (convert lit)

instance Convertable Literal ParsedP AnnotatedP where
  convert TrueLit = TrueLit
  convert FalseLit = FalseLit
  convert (IntLit n) = IntLit n
  convert (FloatLit f) = FloatLit f
  convert (CharLit c) = CharLit c
  convert (TupleLit (l, r)) = TupleLit (convert l, convert r)
  convert EmptyListLit = EmptyListLit

type TypeSubst = [(String, Type)]

type VarEnv = [(String, Type)]
type FunEnv = [(String, [Type], Type)]

-- Generate a fresh variable based on the given namespace "x" and
-- the already generated free variables "vs".
freshVar :: String -> [String] -> String
freshVar x vs = x ++ show (length $ filter (isPrefixOf x) vs)

-- Assigns a type.
annotateType
  :: String                               -- A fresh type variable
  -> [String]                             -- Bookkeeping for generating new *unique* type variables
  -> [(String, Type)]                     -- The current context of type variables to fresh type variables
  -> Maybe Type                           -- The type to assign
  -> ([String], [(String, Type)], Type)   -- A tuple containing (1) the new list of generated variables
                                          --                    (2) the new context
                                          --                    (3) the assigned type
annotateType fv vs ctx t = case t of
  -- In case of Nothing, generate a completely fresh type variable
  Nothing -> (fv:vs, ctx, TypeVar fv)
  -- In case of a type variable,
  --  * check if we have a type for it, 
  --  * otherwise generate one
  Just (TypeVar v) ->
    case lookup v ctx of
      -- We have a type for it
      Just t' -> (vs, ctx, t')
      -- We need to generate a fresh type variable
      Nothing -> (fv:vs, (v, TypeVar fv):ctx, TypeVar fv)
  -- In case of a fully annotated type, use that directly without modifying the context
  Just t' -> (vs, ctx, t')

annotate :: Program ParsedP -> Program AnnotatedP
annotate = annotate' [] []
  where
    annotate'
      :: [String]           -- Bookkeeping for generating new *unique* type variables
      -> [(String, Type)]   -- The context of known type variables
      -> Program ParsedP    -- The declarations to generate an environment for
      -> Program AnnotatedP
    annotate' _ _ [] = []
    annotate' vs ctx (decl:decls) = case decl of
      VarDecl loc name Nothing expr ->
        let fv = freshVar "global" vs in
          let annotatedVar = VarDecl loc name (TypeVar fv) (convert expr) in
            annotatedVar : annotate' (fv : vs) ctx decls
      VarDecl loc name (Just ty) expr ->
        VarDecl loc name ty (convert expr) : annotate' vs ctx decls
      FunDecl loc name returnTy args body ->
        -- Assign a type for the return type.
        let (vs', ctx', annotatedRetTy) = annotateType "f" vs ctx returnTy in
          let annotatedFun = FunDecl loc name annotatedRetTy (annotateArgs vs' ctx' args) (convert <$> body) in
            annotatedFun : annotate' vs' [] decls
        where
          annotateArgs
            :: [String]               -- Bookkeeping for generating new *unique* type variables
            -> [(String, Type)]       -- The context of known type variables 
            -> [(String, Maybe Type)] -- The arguments to annotate
            -> [(String, Type)]
          annotateArgs _ _ [] = []
          annotateArgs vs' ctx' ((argName, curTy):args') = 
            let (vs'', ctx'', annotatedArgTy) = annotateType (freshVar "f" vs) vs' ctx' curTy in
              (argName, annotatedArgTy):annotateArgs vs'' ctx'' args'

typecheck :: Program ParsedP -> Either String (Program TypecheckedP)
typecheck = undefined

typecheckDecl :: Program ParsedP -> Decl ParsedP -> Decl TypecheckedP
typecheckDecl = undefined

unify :: Type -> Type -> Either String TypeSubst
unify IntType IntType = Right []
unify CharType CharType = Right []
unify BoolType BoolType = Right []
unify (TypeVar x) (TypeVar y)
  | x == y = Right []
  | otherwise = Left $ "Cannot unify the type variables " ++ x ++ " and " ++ y ++ "."
unify (TypeVar x) t = if x `elem` tv t
  then Left "Occurs check: cannot construct the infine type."
  else Right [(x, t)]
unify t (TypeVar x) = if x `elem` tv t
  then Left "Occurs check: cannot construct the infine type."
  else Right [(x, t)]
unify (ListType t1) (ListType t2) = unify t1 t2
unify (TupleType s1 s2) (TupleType t1 t2) = do
  u <- unify s1 t1
  unify (subst u s2) (subst u t2)
unify x y = Left $ "Cannot unify " ++ show x ++ " with " ++ show y ++ "."

subst :: TypeSubst -> Type -> Type
subst [] t = t
subst (x:xs) t = subst xs (substSingle t)
  where
    substSingle (TupleType t1 t2) = TupleType (substSingle t1) (substSingle t2)
    substSingle (ListType t') = ListType (substSingle t')
    substSingle (TypeVar v) = case x of
      (v', t') -> if v == v' then t' else TypeVar v
    substSingle t' = t'

-- All the (free) type variables in the given type.
tv :: Type -> [String]
tv (TypeVar x) = [x]
tv (TupleType t1 t2) = tv t1 ++ tv t2
tv (ListType t) = tv t
tv _ = []


