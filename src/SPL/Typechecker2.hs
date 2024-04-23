{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SPL.Typechecker2 where

import qualified Data.Map as Map
import qualified Data.Set as Set


import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State


import SPL.Parser.AST
import SPL.Parser.Parser (showlocation, SourceSpan)
import qualified Text.PrettyPrint as PP
import Data.Maybe (fromMaybe)


data Scheme = Scheme [String] Type

-- ti , -- ti :: TypeEnv → Exp → (Subst, Type) 

type Subst = Map.Map String Type

class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

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


nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Map from function names to the type scheme
newtype FunEnv = FunEnv (Map.Map String Scheme)

instance Types FunEnv where
    ftv (FunEnv env) = ftv (Map.elems env)
    apply s (FunEnv env) = FunEnv (Map.map (apply s) env)



data TIEnv = TIEnv {}
type TIState = Int
type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
    where initTIState = 0

remove :: FunEnv -> String -> FunEnv
remove (FunEnv env) var = FunEnv (Map.delete var env )

newTyVar :: TI Type
newTyVar = do
    s <- get
    put (s + 1)
    return $ TypeVar (reverse $ toTyVar s) False
    where
        toTyVar c | c < 26 = [toEnum (97 + c)]
                  | otherwise = let (n ,r ) = c `divMod` 26 in toEnum (97 + r ) : toTyVar (n - 1)

-- The instantiation function replaces all bound type variables in
--  a type scheme with fresh type variables. 
-- So with this you can make an instance of a type scheme
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t ) = do
    nvars <- mapM (\_ -> newTyVar) vars -- newTyVar for each var in the scheme 
    let s = Map.fromList (zip vars nvars) in return $ apply s t

mgu :: Type -> Type -> TI Subst
mgu (FunType at r) (FunType at' r') = do
    -- We need to unify all the arguments from l with l' pair by pair
    s1 <- zipWithM mgu at at' -- same as: mapM (uncurry mgu) (zip at at') -- No I would never have known that witout the hint thing from vscode
    let s1' = foldr composeSubst nullSubst s1 -- fold all the argument subs together
    s2 <- mgu (apply s1' r) (apply s1' r')
    return (s1' `composeSubst` s2)
mgu (TypeVar u False) t = varBind u t
mgu t (TypeVar u False) = varBind u t
mgu IntType IntType = return nullSubst
mgu BoolType BoolType = return nullSubst
mgu CharType CharType = return nullSubst
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2


varBind :: String -> Type -> TI Subst
varBind u t | t == TypeVar u False = return nullSubst -- idk if this is True or False
            | u `Set.member ` ftv t = throwError $ "occurs check fails: " ++ u ++ " vs. " ++ show t
            | otherwise = return (Map.singleton u t )

generalize :: FunEnv  -> Type -> Scheme
generalize env t = Scheme vars t
    where vars = Set.toList $ ftv t `Set.difference` ftv env

-- Inference --

tiLit :: FunEnv -> Literal ParsedP -> TI (Subst, Type)
tiLit _ (IntLit _) = return (nullSubst, IntType)
tiLit _ TrueLit = return (nullSubst, BoolType)  -- We zouden hier een True of False type kunnen doen das maybe wel cool
tiLit _ FalseLit = return (nullSubst, BoolType)  -- We zouden hier een True of False type kunnen doen das maybe wel cool
tiLit _ (CharLit _) = return (nullSubst, CharType)
tiLit _ EmptyListLit = do 
        var <- newTyVar 
        return (nullSubst, ListType var) -- ListType with typevar inside!
tiLit env (TupleLit (e1, e2)) = do 
    (s1, t1) <- ti env e1
    (s2, t2) <- ti env e2
    return (s1 `composeSubst` s2, TupleType t1 t2)

tc :: FunEnv -> Expr ParsedP -> Type -> TI Subst -- Thanks Quinten
tc env expr given = do
    (s1, inferred) <- ti env expr
    s2 <- mgu given inferred
    return $ s1 `composeSubst` s2

-- Check if the type of 2 expr can be CheckType and then return ResultType
--   ResultType -> CheckType -> ...
tcBinOp :: Type -> Type -> FunEnv -> Expr ParsedP -> Expr ParsedP -> TI (Subst, Type) 
tcBinOp resultT checkT env e1 e2  = do 
    s1 <- tc env e1 checkT
    s2 <- tc env e2 checkT
    return (s1 `composeSubst` s2, resultT) 

-- Checks if the types of 2 expressions match the given type returns and returns the given type 
tcBinOpIdentity :: Type -> FunEnv -> Expr ParsedP -> Expr ParsedP  -> TI (Subst, Type) 
tcBinOpIdentity t = tcBinOp t t

-- Checks if the types of 2 expressions match the given type returns and returns a boolType
tcBinOpBoolean :: Type -> FunEnv -> Expr ParsedP -> Expr ParsedP -> TI (Subst, Type) 
tcBinOpBoolean = tcBinOp BoolType 


-- throwError "Invalid operation at " ++ showlocation meta ++ "\nArguments to " ++ show op ++ "should have types of" ++ show t

ti :: FunEnv -> Expr ParsedP -> TI (Subst, Type)
ti env (LiteralExpr _ lit) = tiLit env lit
ti env (BinOpExpr meta Mul e1 e2) = tcBinOpIdentity IntType env e1 e2
ti env (BinOpExpr meta Mod e1 e2) = tcBinOpIdentity IntType env e1 e2 
ti env (BinOpExpr meta Add e1 e2) = tcBinOpIdentity IntType env e1 e2 
ti env (BinOpExpr meta Div e1 e2) = tcBinOpIdentity IntType env e1 e2 
ti env (BinOpExpr meta Sub e1 e2) = tcBinOpIdentity IntType env e1 e2 
    -- These next ones are polymorf, they can either be char or int
ti env (BinOpExpr meta Gt e1 e2) = do {tcBinOpBoolean IntType env e1 e2} 
                                   `catchError` \_ -> tcBinOpIdentity CharType env e1 e2 
                                   `catchError` \_ -> throwError $ "Invalid operation at " ++ showlocation meta ++ "\nArguments to > should be either Int or Char"
ti env (BinOpExpr meta Gte e1 e2) =  do {tcBinOpBoolean IntType env e1 e2} 
                                     `catchError` \_ -> tcBinOpIdentity CharType env e1 e2  
                                     `catchError` \_ -> throwError $ "Invalid operation at " ++ showlocation meta ++ "\nArguments to >= should be either Int or Char"
ti env (BinOpExpr meta Lt e1 e2) =  do {tcBinOpBoolean IntType env e1 e2 } 
                                    `catchError` \_ -> tcBinOpIdentity CharType env e1 e2 
                                    `catchError` \_ -> throwError $ "Invalid operation at " ++ showlocation meta ++ "\nArguments to < should be either Int or Char"
ti env (BinOpExpr meta Lte e1 e2) = do {tcBinOpBoolean IntType env e1 e2 } 
                                    `catchError` \_ -> tcBinOpBoolean CharType env e1 e2 
                                    `catchError` \_ -> throwError $ "Invalid operation at " ++ showlocation meta ++ "\nArguments to <= should be either Int or Char"
ti env (BinOpExpr meta Eq e1 e2) = do 
                                     (s1, t1) <- ti env e1 
                                     (s2, t2) <- ti env e1 
                                     s3 <- tc env e1 t2 -- Check if they are the same?
                                     s4 <- tc env e2 t1 
                                    --  s5 <- mgu t1 t2 -- Moet dit ook nog?
                                     let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
                                     return (s, t1) 
ti env (BinOpExpr meta Neq e1 e2) = tcBinOpIdentity BoolType env e1 e2
ti env (BinOpExpr meta And e1 e2) = tcBinOpIdentity BoolType env e1 e2
ti env (BinOpExpr meta Or e1 e2) = tcBinOpIdentity BoolType env e1 e2
ti env (BinOpExpr meta Cons e1 e2) = do 
                                        (s1, t1) <- ti env e1 
                                        (s2, t2) <- ti env e2 
                                        s3 <- tc env e2 (ListType t1)
                                        let s = s1 `composeSubst` s2 `composeSubst` s3
                                        return (s, ListType t1)

ti env (UnaryOpExpr _ Negate e) = do
        (s1, t) <- ti env e
        s2 <- mgu BoolType t -- Check if its a bool, again here we could actually negate the bool maybe, like dependent types but only for bools?
        return (s1 `composeSubst` s2, t)
ti env (UnaryOpExpr _ (FieldAccess field) expr) = undefined
ti funenv@(FunEnv env) (AssignExpr meta (Identifier var _) expr) = do 
                                       (s, t) <- ti funenv expr
                                       fresh <- newTyVar -- take fresh value as default 
                                       let storedschema = fromMaybe (Scheme [] fresh) (Map.lookup var env)
                                       storedtype <- instantiate storedschema
                                       s' <- mgu storedtype t
                                       return (s `composeSubst` s', t)
-- Todo Variables can have fields but I am just going to take the type of the String, I am leaving the warning
ti (FunEnv env) (VariableExpr _ (Identifier var field)) = case Map.lookup var env of 
                                        Nothing -> throwError $ "unbound variable: " ++ var
                                        Just sigma -> do t <- instantiate sigma
                                                         return (nullSubst, t)
ti env (FunctionCallExpr _ name args) = undefined -- How do we get the types of the arguments from the decl?
