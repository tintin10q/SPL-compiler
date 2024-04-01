{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Parser where

import SPL.Parser.AST
import SPL.Parser.Parser (SourceSpan)

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

typecheck :: Program ParsedP -> Either String (Program TypecheckedP)
typecheck = undefined

typecheckDecl :: Decl ParsedP -> Decl TypecheckedP
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
