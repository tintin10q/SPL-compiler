{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
module SPL.Typechecker2 where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State

import SPL.Parser.AST
import Data.Maybe (fromMaybe)
import SPL.Parser.Parser (SourceSpan)


data Scheme = Scheme [String] Type

type FunEnv = Map.Map String Scheme
type VarEnv = Map.Map String Type
type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

data TIEnv = TIEnv {}
type TIState = Int
type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
    where initTIState = 0

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    return $ TypeVar (reverse $ toTyVar s) False
    where toTyVar c | c < 26 = [toEnum (97 + c)]
                    | otherwise = let (n ,r ) = c `divMod` 26 in toEnum (97 + r) : toTyVar (n - 1)

class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

class Typecheck a where
    {-# MINIMAL tc | ti #-}
    tc :: (FunEnv, VarEnv) -> a -> Type -> TI Subst
    tc env a t = do
        (s1, inferredT) <- ti env a
        s2 <- unify t inferredT
        return $ s1 `composeSubst` s2
    ti :: (FunEnv, VarEnv) -> a -> TI (Subst, Type)
    ti env a = do
        t <- newTyVar
        s <- tc env a t
        return (s, apply s t)

instance Types Type where
    ftv (TupleType t1 t2) = Set.union (ftv t1) (ftv t2)
    ftv (ListType  t) = ftv t
    ftv (FunType args rt) = Set.union (ftv args) (ftv rt)
    ftv (TypeVar var False) = Set.singleton var
    ftv _ = Set.empty -- No rigid!
    apply subst t@(TypeVar var False) = fromMaybe t (Map.lookup var subst)
    apply subst (TupleType t1 t2) = TupleType  (apply subst t1) (apply subst t2)
    apply subst (ListType  t) = ListType (apply subst t)
    apply subst (FunType args rt) = FunType (apply subst args) (apply subst rt)
    apply _ t = t

instance Types Scheme where
    ftv (Scheme vars t ) = ftv t `Set.difference` Set.fromList vars
    apply s (Scheme vars t ) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv = foldr (Set.union . ftv) Set.empty

instance Types FunEnv where
    ftv env = ftv (Map.elems env)
    apply s = Map.map (apply s)

-- The instantiation function replaces all bound type variables in
--  a type scheme with fresh type variables. 
-- So with this you can make an instance of a type scheme
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (const newTyVar) vars -- newTyVar for each var in the scheme 
    let s = Map.fromList (zip vars nvars) in
        return $ apply s t

varBind :: String -> Type -> TI Subst
varBind u t | u `Set.member` ftv t = throwError $
                "Occurs check: cannot construct the infinite type " ++ u ++ " ~ " ++ show t
            | otherwise = return (Map.singleton u t)

unify :: Type -> Type -> TI Subst
unify (FunType at r) (FunType at' r') = do
    -- We need to unify all the arguments from l with l' pair by pair
    s1 <- zipWithM unify at at' -- same as: mapM (uncurry unify) (zip at at') -- No I would never have known that witout the hint thing from vscode
    let s1' = foldr composeSubst nullSubst s1 -- fold all the argument subs together
    s2 <- unify (apply s1' r) (apply s1' r')
    return $ s1' `composeSubst` s2
unify (TypeVar u False) t = varBind u t
unify t (TypeVar u False) = varBind u t
unify (TypeVar u True) t = throwError $
    "Cannot unify " ++ u ++ " with " ++ show t ++ ", as " ++ u ++ " is a rigid type variable."
unify t (TypeVar u True) = throwError $
    "Cannot unify " ++ u ++ " with " ++ show t ++ ", as " ++ u ++ " is a rigid type variable."
unify (ListType t1) (ListType t2) = unify t1 t2
unify (TupleType t1 t2) (TupleType t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return $ s1 `composeSubst` s2
unify IntType IntType = return nullSubst
unify BoolType BoolType = return nullSubst
unify CharType CharType = return nullSubst
unify t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

instance Typecheck (Literal ParsedP) where
    ti _env (IntLit _) = return (nullSubst, IntType)
    ti _env TrueLit = return (nullSubst, BoolType)
    ti _env FalseLit = return (nullSubst, BoolType)
    ti _env (CharLit _) = return (nullSubst, CharType)
    ti _env EmptyListLit = do
            var <- newTyVar
            return (nullSubst, ListType var) -- ListType with typevar inside!
    ti env (TupleLit (e1, e2)) = do
        (s1, t1) <- ti env e1
        (s2, t2) <- ti env e2
        return (s1 `composeSubst` s2, TupleType t1 t2)

-- Check if the type of 2 expr can be CheckType and then return ResultType
--   ResultType -> CheckType -> ...
tcBinOp :: Type -> Type -> (FunEnv, VarEnv) -> Expr ParsedP -> Expr ParsedP -> TI (Subst, Type)
tcBinOp resultT checkT env e1 e2  = do
    s1 <- tc env e1 checkT
    s2 <- tc env e2 checkT
    return (s1 `composeSubst` s2, resultT)

-- Checks if the types of 2 expressions match the given type returns and returns the given type 
tcBinOpIdentity :: Type -> (FunEnv, VarEnv) -> Expr ParsedP -> Expr ParsedP  -> TI (Subst, Type)
tcBinOpIdentity t = tcBinOp t t

-- Checks if the types of 2 expressions match the given type returns and returns a boolType
tcBinOpBoolean :: Type -> (FunEnv, VarEnv) -> Expr ParsedP -> Expr ParsedP -> TI (Subst, Type)
tcBinOpBoolean = tcBinOp BoolType

instance Typecheck (Expr ParsedP) where
    ti env (LiteralExpr _ lit) = ti env lit
    ti env (BinOpExpr _meta Mul e1 e2) = tcBinOpIdentity IntType env e1 e2
    ti env (BinOpExpr _meta Mod e1 e2) = tcBinOpIdentity IntType env e1 e2
    ti env (BinOpExpr _meta Add e1 e2) = tcBinOpIdentity IntType env e1 e2
    ti env (BinOpExpr _meta Div e1 e2) = tcBinOpIdentity IntType env e1 e2
    ti env (BinOpExpr _meta Sub e1 e2) = tcBinOpIdentity IntType env e1 e2
        -- These next ones are polymorf, they can either be char or int
    ti env (BinOpExpr meta Gt e1 e2) = do {tcBinOpBoolean IntType env e1 e2}
                                    `catchError` \_ -> tcBinOpIdentity CharType env e1 e2
                                    `catchError` \_ -> throwError $ "Invalid operation at " ++ show (meta :: SourceSpan) ++ "\nArguments to > should be either Int or Char"
    ti env (BinOpExpr meta Gte e1 e2) =  do {tcBinOpBoolean IntType env e1 e2}
                                        `catchError` \_ -> tcBinOpIdentity CharType env e1 e2
                                        `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to >= should be either Int or Char"
    ti env (BinOpExpr meta Lt e1 e2) =  do {tcBinOpBoolean IntType env e1 e2 }
                                        `catchError` \_ -> tcBinOpIdentity CharType env e1 e2
                                        `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to < should be either Int or Char"
    ti env (BinOpExpr meta Lte e1 e2) = do {tcBinOpBoolean IntType env e1 e2 }
                                        `catchError` \_ -> tcBinOpBoolean CharType env e1 e2
                                        `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to <= should be either Int or Char"
    ti env (BinOpExpr _meta Eq e1 e2) = do
                                        (s1, t1) <- ti env e1
                                        (s2, t2) <- ti env e2
                                        s3 <- unify t1 t2
                                        let s = s1 `composeSubst` s2 `composeSubst` s3
                                        return (s, t1)
    ti env (BinOpExpr _meta Neq e1 e2) = tcBinOpIdentity BoolType env e1 e2
    ti env (BinOpExpr _meta And e1 e2) = tcBinOpIdentity BoolType env e1 e2
    ti env (BinOpExpr _meta Or e1 e2) = tcBinOpIdentity BoolType env e1 e2
    ti env (BinOpExpr meta Cons e1 e2) = do
                                            (s1, t1) <- ti env e1
                                            (s2, t2) <- ti env e2
                                            s3 <- case t2 of
                                                    ListType u1 -> unify t1 u1
                                                    _ -> throwError $
                                                        "Couldn't match expected type " ++ show (ListType t1) ++
                                                        " with " ++ show t2 ++ " at " ++ show meta ++
                                                        ". You tried to cons " ++ show t1 ++ " with " ++ show t2 ++
                                                        ", but this is not legal."
                                            let s = s1 `composeSubst` s2 `composeSubst` s3
                                            return (s, ListType t1)

    ti env (UnaryOpExpr _ Negate e) = do
            (s1, t) <- ti env e
            s2 <- unify BoolType t -- Check if its a bool, again here we could actually negate the bool maybe, like dependent types but only for bools?
            return (s1 `composeSubst` s2, t)
    ti _env (UnaryOpExpr _ (FieldAccess _field) _expr) = undefined
    ti env@(_, varenv) (AssignExpr meta (Identifier var _) expr) = do
                                        (s, t) <- ti env expr
                                        let varTM = Map.lookup var varenv
                                        varT <- case varTM of
                                                Nothing -> throwError $ "Undefined variable " ++ show var ++ " at " ++ show meta ++ "."
                                                Just varT -> return varT
                                        s' <- unify varT t
                                        return (s `composeSubst` s', varT)
    -- Todo Variables can have fields but I am just going to take the type of the String, I am leaving the warning
    ti (funenv, _) (VariableExpr _ (Identifier var _field)) = case Map.lookup var funenv of
                                            Nothing -> throwError $ "unbound variable: " ++ var
                                            Just sigma -> do
                                                t <- instantiate sigma
                                                return (nullSubst, t)
    ti env@(funenv, _) (FunctionCallExpr _ funcName args) = do
        let schemeM = Map.lookup funcName funenv
        scheme <- case schemeM of
                    Nothing -> throwError $ "Function " ++ funcName ++ " undefined."
                    Just scheme -> return scheme
        funType <- instantiate scheme
        case funType of
            FunType argsT rT -> do
                s1 <- zipWithM (tc env) args argsT
                let s = foldr composeSubst nullSubst s1
                return (s, apply s rT)
            _ -> error $ "Function environment contains something other than a FunType: " ++ show funType ++ "!!!!!"
