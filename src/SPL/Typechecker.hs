{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Typechecker where

import SPL.Parser.AST
import SPL.Parser.Parser (SourceSpan)
import Data.List

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

type TypeSubst = [(String, Type)]

type VarEnv = [(String, Type)]
type FunEnv = [(String, [Type], Type)]

-- Generate a fresh variable based on the given namespace "x" and
-- the already generated free variables "vs".
freshVar :: String -> [String] -> String
freshVar x vs = x ++ show (length $ filter (isPrefixOf x) vs)

-- Assigns a list of types (reflexive transitive closure (almost) of "assignTypes").
assignTypes
  :: String                               -- The namespace to use for generating fresh variables
  -> [String]                             -- Bookkeeping for generating new *unique* type variables
  -> [(String, Type)]                     -- The context of known type variables
  -> [Maybe Type]                         -- The type to assign
  -> ([String], [(String, Type)], [Type]) -- A tuple containing (1) the new context
                                          --                    (2) the new list of generated variables
                                          --                    (3) the assigned types
assignTypes _ vs ctx [] = (vs, ctx, [])
assignTypes ns vs ctx (t:ts) =
  let (vs', ctx', t')    = assignType (freshVar ns vs) vs ctx t
      (vs'', ctx'', ts') = assignTypes ns vs' ctx' ts
  in (vs'', ctx'', t':ts')

-- Assigns a type.
assignType
  :: String                               -- A fresh type variable
  -> [String]                             -- Bookkeeping for generating new *unique* type variables
  -> [(String, Type)]                     -- The current context of type variables to fresh type variables
  -> Maybe Type                           -- The type to assign
  -> ([String], [(String, Type)], Type)   -- A tuple containing (1) the new context
                                          --                    (2) the new list of generated variables
                                          --                    (3) the assigned type
assignType fv vs ctx t = case t of
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

-- Generate the environment of function declarations.
funEnv :: Program p -> FunEnv
funEnv = funEnv' [] []
  where
    funEnv'
      :: [String]         -- Bookkeeping for generating new *unique* type variables
      -> [(String, Type)] -- The context of known type variables
      -> Program p        -- The declarations to generate an environment for
      -> FunEnv
    funEnv' _ _ [] = []
    funEnv' vs ctx (decl:decls) = case decl of
      -- Skip in case of VarDecl
      VarDecl{} -> funEnv' vs ctx decls
      FunDecl _ name returnTy args _stmts ->
        -- Assign a type for the return type.
        let (vs', ctx', ts) = assignTypes "f" vs ctx (returnTy:map snd args)
        in (name, tail ts, head ts):funEnv' vs' ctx' decls

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


