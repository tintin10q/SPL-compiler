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

-- Generate the environment of function declarations. The first argument,
-- "vs" is the list of type variables, and the second argument the program
-- to get the declarations of.
funEnv :: Program p -> FunEnv
funEnv = funEnv' [] []
  where
    funEnv'
      :: [(String, String)] -- The context of type variables to types
      -> [String]           -- The generated type variables, used for generating new unique ones
      -> Program p          -- The declarations to generate an environment for
      -> FunEnv
    funEnv' _ _ [] = []
    funEnv' ctx vars (d:ds) = case d of
      -- Skip in case of VarDecl
      VarDecl{} -> funEnv' ctx vars ds
      FunDecl _ name returnTy args _ ->
        -- Assign a type for the return type.
        let (ctx', vars', returnTy') = assignType (freshVar "f") ctx vars returnTy
            (ctx'', vars'', argTys) = assignTypes (freshVar "f") ctx' vars' (map snd args)
        in (name, argTys, returnTy'):funEnv' ctx'' vars'' ds

-- The reflexive transitive closure of "assignType".
assignTypes
  :: ([String] -> String)
  -> [(String, String)]
  -> [String]
  -> [Maybe Type]
  -> ([(String, String)], [String], [Type])
assignTypes _ ctx vs [] = (ctx, vs, [])
assignTypes fresh ctx vs (t:ts) =
  let (ctx', vs', t') = assignType fresh ctx vs t
      (ctxFinal, vsFinal, ts') = assignTypes fresh ctx' vs' ts
  in (ctxFinal, vsFinal, t':ts')

-- Assigns a type to the given Maybe Type
assignType
  :: ([String] -> String)                 -- A function for generating new unique type variables
  -> [(String, String)]                   -- The current context of type variables to fresh type variables
  -> [String]                             -- The list of generated type variables (used for generating new *unique* variables)
  -> Maybe Type                           -- The Maybe Type to generate a type for
  -> ([(String, String)], [String], Type) -- A tuple containing (1) the new context
                                          --                    (2) the new list of generated variables
                                          --                    (3) the assigned type
assignType fresh ctx vs t = case t of
  -- In case of Nothing, generate a completely fresh type variable
  Nothing -> 
    let fv = fresh vs
    in (ctx, fv:vs, TypeVar fv)
  -- In case of a type variable, check if we have a fresh type variable for it, otherwise generate one
  Just (TypeVar v) ->
    case lookup v ctx of
      Just v' -> (ctx, vs, TypeVar v')
      Nothing ->
        let fv = fresh vs
        in ((v, fv):ctx, fv:vs, TypeVar fv)
  -- In case of a fully annotated type, use that directly without modifying the context
  Just t' -> (ctx, vs, t')

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


