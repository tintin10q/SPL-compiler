{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

module SPL.Typechecker2 where


import SPL.AST
import SPL.Colors (blue, red, bold, black, green)
import SPL.Parser.SourceSpan (SourceSpan, endPos, showEnd, showStart)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Megaparsec (SourcePos (SourcePos), unPos)
import SPL.PrettyPrint (prettyPrintMap, printWithCommas, pretty)
import Data.List (intercalate)

data Scheme = Scheme [String] Type
  deriving (Show, Eq)

type FunEnv = Map.Map String Scheme

type VarEnv = Map.Map String Type

{- A subst is a map from type var names to types-}
type Subst = Map.Map String Type

type FreeTypeVarNames = Set.Set String

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

data TIEnv = TIEnv {}

type TIState = Int

type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
  where
    initTIState = 0

newTyVar :: TI Type
newTyVar = do
  s <- get
  put (s + 1)
  return $ TypeVar (reverse $ toTyVar s) False
  where
    toTyVar c
      | c < 26 = [toEnum (97 + c)]
      | otherwise = let (n, r) = c `divMod` 26 in toEnum (97 + r) : toTyVar (n - 1)

class Types a where
  ftv :: a -> FreeTypeVarNames
  apply :: Subst -> a -> a


data TypecheckEnv = TypecheckEnv { getFunenv :: FunEnv,
                                   getVarenv :: VarEnv,
                                   getMeta :: SourceSpan,
                                   getFunName :: Maybe String }
    deriving Show

class Typecheck a where
  {-# MINIMAL tc | ti #-}
  tc :: TypecheckEnv -> a -> Type -> TI Subst
  tc env a t = do
    (s1, inferredT) <- ti env a
    s2 <- unify env t inferredT
    return $ s1 `composeSubst` s2
  ti :: TypecheckEnv -> a -> TI (Subst, Type)
  ti env a = do
    t <- newTyVar
    s <- tc env a t
    return (s, apply s t)

instance Types Type where
  ftv (TupleType t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv (ListType t) = ftv t
  ftv (FunType args rt) =  ftv args <> ftv rt
  ftv (TypeVar var False) = Set.singleton var
  ftv _ = Set.empty -- No rigid!
  apply subst t@(TypeVar var False) = case Map.lookup var subst of
                                          Nothing -> t; -- Return the same type variable  
                                          Just concrete -> concrete -- Replace type variable with the type of the sub
  apply subst (TupleType t1 t2) = TupleType (apply subst t1) (apply subst t2)
  apply subst (ListType t) = ListType (apply subst t)
  apply subst (FunType args rt) = FunType (apply subst args) (apply subst rt)
  apply _ concreteType = concreteType

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t) -- make a new scheme by applying the sub, todo idk why the delete though

instance (Types a) => Types [a] where
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
  let s = Map.fromList (zip vars nvars)
   in return $ apply s t

varBind :: SourceSpan -> String -> Type -> TI Subst
varBind meta u t
  | u `Set.member` ftv t =
      throwError $
        "Occurs check: cannot construct the infinite type " ++ u ++ " ~ " ++ show t ++ " at " ++ showStart meta
  | otherwise = return (Map.singleton u t)

-- todo, we could also include variables names here maybe
-- Based on the variables in the varenv we could do suggestions of types to use!!
unify :: TypecheckEnv -> Type -> Type -> TI Subst
unify env (FunType args r) (FunType args' r') = do
  -- We need to unify all the arguments from l with l' pair by pair
  let unify' = unify env
  s1 <- zipWithM unify' args args' -- same as: mapM (uncurry unify) (zip at at') -- No I would have never have known that witout the hint thing from vscode
  let s1' = foldr composeSubst nullSubst s1

  s2 <- unify env (apply s1' r) (apply s1' r')
  return $ s1' `composeSubst` s2
unify env (TypeVar u False) t = varBind (getMeta env) u t
unify env t (TypeVar u False) = varBind (getMeta env) u t
unify env ty1@(TypeVar name1 True) ty2@(TypeVar name2 True) =
  if name1 == name2
    then return nullSubst
    else throwError $ "Cannot unify " ++ show ty1 ++ " with " ++ show ty2 ++ " as '" ++ blue name1 ++ "' and '" ++ blue name2 ++ "' are two "++bold "different"++" rigid type variable because they have a different name.\nA rigid type variable means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ formatUnifyError ty2 env
unify env ty@(TypeVar u True) t =
  throwError $
    "Cannot unify " ++ show ty ++ " with " ++ show t ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ formatUnifyError t env
unify env t ty@(TypeVar u True) =
  throwError $
    "Cannot unify " ++ show t ++ " with " ++ show ty ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ formatUnifyError t env
unify env (ListType t1) (ListType t2) = unify env t1 t2
unify env (TupleType t1 t2) (TupleType t1' t2') = do
  s1 <- unify env t1 t1'
  s2 <- unify env (apply s1 t2) (apply s1 t2')
  return $ s1 `composeSubst` s2
unify _ IntType IntType = return nullSubst
unify _ BoolType BoolType = return nullSubst
unify _ CharType CharType = return nullSubst
-- todo probably add case for Void Void 
unify env t1 t2 = throwError $ "Types do not unify:\n" ++ show t1 ++ " vs. " ++ show t2 ++ formatUnifyError t1 env

instance Typecheck (Literal ReturnsCheckedP) where
  ti _ (IntLit _) = return (nullSubst, IntType)
  ti _ TrueLit = return (nullSubst, BoolType)
  ti _ FalseLit = return (nullSubst, BoolType)
  ti _ (CharLit _) = return (nullSubst, CharType)
  ti _ EmptyListLit = do
    var <- newTyVar
    return (nullSubst, ListType var) -- ListType with typevar inside!
  ti env (TupleLit (e1, e2)) = do
    (s1, t1) <- ti env e1
    (s2, t2) <- ti env e2
    return (s1 `composeSubst` s2, TupleType t1 t2)

-- Check if the type of 2 expr can be CheckType and then return ResultType
--   ResultType -> CheckType -> ...
tcBinOp :: Type -> Type -> TypecheckEnv -> Expr ReturnsCheckedP  -> Expr ReturnsCheckedP  -> TI (Subst, Type)
tcBinOp resultT checkT env e1 e2 = do
  s1 <- tc env e1 checkT
  s2 <- tc env e2 checkT
  return (s1 `composeSubst` s2, resultT)

-- todo Dependent types for booleans

{- Check if the infered types of 2 expr can be unified with eachother and then return ResultType as the type of the 2 expressions
We are checking if the two types of the expressions are equal in a sense -}
tcBinOpEqual :: Type -> TypecheckEnv -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpEqual resultT env e1 e2 = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti env e2
  s3 <- unify env t1 t2
  return (s1 `composeSubst` s2 `composeSubst` s3, resultT)

{- Checks if the types of 2 expressions match the given type and returns the given type as the type of the expression -}
tcBinOpIdentity :: Type -> TypecheckEnv -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpIdentity t = tcBinOp t t

-- Checks if the types of 2 expressions match the given type returns and returns a boolType
tcBinOpBoolean :: Type -> TypecheckEnv -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP-> TI (Subst, Type)
tcBinOpBoolean = tcBinOp BoolType

instance Typecheck (Expr ReturnsCheckedP) where
  ti env (LiteralExpr meta lit) = ti (env {getMeta = meta}) lit
  ti env (BinOpExpr meta Mul e1 e2) = tcBinOpIdentity IntType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Mod e1 e2) = tcBinOpIdentity IntType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Add e1 e2) = tcBinOpIdentity IntType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Div e1 e2) = tcBinOpIdentity IntType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Sub e1 e2) = tcBinOpIdentity IntType (env {getMeta = meta}) e1 e2
  -- These next ones are polymorf, they can either be char or int
  ti env (BinOpExpr meta Gt e1 e2) = do
    let env' = env {getMeta = meta}
    (_, t1) <- ti env' e1
    (_, t2) <- ti env' e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean IntType env' e1 e2
      `catchError` \_ ->
        tcBinOpBoolean CharType env' e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to > should be either Int or Char but you gave " ++ you_gave
  ti env (BinOpExpr meta Gte e1 e2) = do
    let env' = env {getMeta = meta}
    (_, t1) <- ti env' e1
    (_, t2) <- ti env' e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean IntType env' e1 e2
      `catchError` \_ ->
        tcBinOpBoolean CharType env' e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to >= should be either Int or Char but you gave " ++ you_gave
  ti env (BinOpExpr meta Lt e1 e2) = do
    let env' = env {getMeta = meta}
    (_, t1) <- ti env' e1
    (_, t2) <- ti env' e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean IntType env' e1 e2
      `catchError` \_ ->
        tcBinOpBoolean CharType env' e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to < should be either Int or Char but you gave " ++ you_gave
  ti env (BinOpExpr meta Lte e1 e2) = do
    let env' = env {getMeta = meta}
    (_, t1) <- ti env' e1
    (_, t2) <- ti env' e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean IntType env' e1 e2
      `catchError` \_ ->
        tcBinOpBoolean CharType env' e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to <= should be either Int or Char but you gave " ++ you_gave
  ti env (BinOpExpr meta Eq e1 e2) = tcBinOpEqual BoolType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Neq e1 e2) = tcBinOpEqual BoolType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta And e1 e2) = tcBinOpIdentity BoolType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Or e1 e2) = tcBinOpIdentity BoolType (env {getMeta = meta}) e1 e2
  ti env (BinOpExpr meta Cons e1 e2) = do
    let env' = env {getMeta = meta}
    (s1, t1) <- ti env' e1
    (s2, t2) <- ti env' e2
    -- Maybe we should see if we can tc with a list that has a type var inside
    _ <- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++  " s2: " ++ show s2 ) pure ()
    s3 <- case t2 of
            -- If t2 is a list type we have to unify with the type inside the list
            ListType u1 -> unify env' t1 u1  `catchError` \err -> throwError $ "You tried to cons " ++ show t1 ++ " with " ++ show t2 ++ ", but this is not legal.\n" ++ err
            -- Else just try to unify and let it fail (most likely)
            _ -> unify env' t1 t2
    -- Todo ask is this needed?
    -- newTy <- newTyVar

    let free = Set.toList $ ftv t2
        s4 = Map.fromList $ (\tyvarname -> (tyvarname, t1)) <$> free

    let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
    -- _ <- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++ show s4 ++ " s4!" ++ "s: " ++ show s ) pure ()
    return (s, ListType t1) -- This is good because we always get a list type like this and we are sure we can put t1 inside it 
  ti env (UnaryOpExpr meta Negate e) = do
    let env' = env {getMeta = meta}
    (s1, t) <- ti env' e
    s2 <- unify env' BoolType t -- Check if its a bool, again here we could actually negate the bool maybe, like dependent types but only for bools?
    return (s1 `composeSubst` s2, t)
  ti _env (UnaryOpExpr _ (FieldAccess _field) _expr) = error "Field access is not working yet :]"
  -- Todo Variables can have fields but I am just going to take the type of the String, I am leaving the warning
  ti env (VariableExpr meta (Identifier var _field)) = case Map.lookup var $ getVarenv env of
    Nothing -> throwError $ "Unbound variable: '" ++ blue var ++ "' at " ++ showStart meta
    Just sigma -> pure (nullSubst, sigma)
  ti env (FunctionCallExpr meta funcName args) = do
    let schemeM = Map.lookup funcName $ getFunenv env
    scheme <- case schemeM of
      Nothing -> throwError $ "Function '" ++ blue funcName ++ "' is undefined. at " ++ showStart meta
      Just scheme -> return scheme
    let Scheme scheme_args scheme_retty = scheme
    -- Check number of args 
    let scheme_arg_count = length scheme_args
        n_given_args = length args
    when (n_given_args > scheme_arg_count) (throwError $ red "You called the '"++ blue funcName ++ red "' function at " ++ showStart meta ++ red " with too many arguments " ++ "("++ green ( show n_given_args )++ " > " ++ green (show scheme_arg_count) ++ ")" ++ "\nThe '"++ blue funcName ++ "' function takes " ++ green ( show scheme_arg_count) ++ " argument" ++ (if scheme_arg_count == 1 then "" else "s") ++ " but you gave " ++ green ( show n_given_args) ++ " argument" ++ (if n_given_args == 1 then "" else "s") ++".")
    when (n_given_args < scheme_arg_count) (let missing =scheme_arg_count - n_given_args in throwError $ red "Function call at " ++ showStart meta ++ red " is missing " ++ green (show missing) ++ red " arguments:"++"\nThe arguments are called " ++ intercalate " and " (map blue (drop n_given_args scheme_args)) ++ ".")


    -- How do we get the types of the function argumens of other functions?

    -- If we get here we did find the function!
    retType <- instantiate scheme
    checkedRetType <- case retType of
          FunType {} -> error $ "Got a funtype after instantiate scheme, how does that happen!! We don't support functions returning functions" ++ showStart meta
          ty -> Debug.trace (black "Instanciated function it gave " ++ show retType) pure ty 
          {- FunType argsT rT -> do
            s1 <- zipWithM (tc env meta) args argsT
            let s = foldr composeSubst nullSubst s1
            return (s, apply s rT) -}
          -- _ -> error $ "Function environment contains something weird " ++ show retType ++ "!!!!!" ++ showStart meta
    return (nullSubst, checkedRetType)

-- Maybe we also do this for list of expressions but probably not because a list of expressions does not make sense
instance Typecheck [Stmt ReturnsCheckedP] where

  {- todo this is wrong because it does not update the envrioment every time but uses the one from at the start! 
       I guess thats ok for statement and below because they can't change env. But lets just make this one only for statements -}
  ti _ [] = return (nullSubst, VoidType)
  ti env [stmt] = ti env stmt -- This fixes that if you have a return statement as the last statement that the last will be compared with void because [] = VoidType
  ti env (stmt:later) = do
    (mysub, mytype) <- ti env stmt
    (latersub, othertype) <- ti env later
    let sub = mysub `composeSubst` latersub
    returned_ty <- case (mytype, othertype) of
                        -- I think we have to have these cases so that unify does not have to deal with VoidType
                        (VoidType, VoidType) -> pure VoidType
                        -- If retty1 is not Void and the rest is void it means that I return something! In that case just propagete what I return
                        (retty1, VoidType) -> pure retty1
                        -- If retty1 is Void and the rest is not void it means that I do not return something but later we do! In that case just propagete what we return later
                        (VoidType, retty2) -> pure retty2 -- I think we have to have this case so that unify does not have to deal with VoidType
                        (retty1, retty2) -> (unify env retty1 retty2 `catchError` \err -> throwError $  red " Could not unify return type of statement block at " ++ showStart (getMeta env) ++ "\n" ++ err)
                                                >> pure retty2 -- go with othertype, the type of the end of the stmt! Thats where the return is. 
    -- Uhm 
    return (sub, returned_ty)

-- This could also be different version of ti in the typeclass that you don't have to implement??

-- todo The type of a list of statements is the return type!!!!! We also stop after the return! Ignoring other statements.
-- A step where we remove the things after return statements in the same list. Simple dead code removal.

instance Typecheck (Stmt ReturnsCheckedP) where
  ti env (ReturnStmt meta (Just expr)) = do

    let env' = env {getMeta = meta}
    (s1, expr_type) <- ti env' expr
    let funenv = getFunenv env

    let funname = case getFunName env' of
                    Just name -> name
                    Nothing -> error $ red "??? No function name, that means the return at "++ showStart meta ++ red " is outside a function? Don't do that."
    let   (Scheme _ return_type) = funenv Map.! funname
    s2 <- unify env expr_type return_type `catchError` \err -> throwError $ red "Invalid return type in function '" ++ blue funname ++ red "'"++" at " ++ showEnd meta ++ "\n" ++ err
    return (s1 `composeSubst` s2, expr_type)
  ti _ (ReturnStmt _ Nothing) = return (nullSubst, VoidType)
  ti env (IfStmt meta cond consequent (Just alternative)) =
    do
      -- _ <- Debug.trace "checking if else" pure ()
      let env' = env {getMeta = meta}
      s1 <- tc env cond BoolType
      (s2, ty1) <- ti env' consequent
      (s3, ty2) <- ti env' alternative
      s4 <- unify env' ty1 ty2
      let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
      return (s, ty1) -- we can do ty1 because of the previous returns checks, we know we can just pick one
  ti env (IfStmt meta cond consequent Nothing) =
    do
      -- _ <- Debug.trace "checking if " pure ()
      let env' = env {getMeta = meta}
      s1 <- tc env' cond BoolType
      (s2, ty) <- ti env' consequent
      let s = s1 `composeSubst` s2
      return (s, ty)
  -- todo add fields
  ti env (AssignStmt meta (Identifier var _) expr) = do
    -- _ <- Debug.trace "checking assing" pure ()
    let env' = env {getMeta = meta}
    (s, t) <- ti env' expr
    let varTM = Map.lookup var (getVarenv env')
    varT <- case varTM of
      Nothing -> throwError $ "Undefined variable " ++ show var ++ " at " ++ show meta ++ "."
      Just varT -> return varT
    s' <- unify env' varT t
    return (s `composeSubst` s', VoidType) -- This is void type because its never a return
  ti env (WhileStmt meta cond stmts) =
    do
      -- _ <- Debug.trace "checking while" pure ()
      let env' = env {getMeta = meta}
      s1 <- tc env' cond BoolType
      (s2, ty) <- ti env' stmts
      let s = s1 `composeSubst` s2
      return (s, ty)
  ti env (ExprStmt meta e) = ti (env {getMeta = meta}) e >>= \(sup, _) -> pure (sup, VoidType) -- Also void type because its not a return
  -- ti env meta (BlockStmt stmt) = Debug.trace "ti called on BlackStmt!! That should not happen right?" ti env meta stmt
  ti _env (BlockStmt _body) = error "ti called on BlackStmt!! That should not happen right during type checking?" -- but when we do stop at return?

-- todo remove the debug.trace

{-

The problem with this is that we don't need to actually combine the subs from these but if one is wrong we need to know it.
  OHHHH We return a fun type ofcourse!! From that we can upgrade the decl in the wrapper function!

-}
instance Typecheck (Decl ReturnsCheckedP) where
  ti env (VarDecl meta _name _ expr) = ti (env {getMeta = meta}) expr
  -- We do not assume that the caller added anything from this fundecl to the env that the fun args and self function are already in the env!
      -- The returned sub contains the information of the args, decls and body
  ti env (FunDecl meta name retty args fundelcs body) = do
      _ <- Debug.trace (blue ("Inferencing function '" ++ name ++ "': ")) pure ()
      let (inital_funenv, initial_varenv) = (getFunenv env, getVarenv env)

      initial_return_type <- maybe newTyVar pure retty

      -- Add the args to the varenv
      inital_arg_types <- mapM (maybe newTyVar pure . snd) args
      let argnames = map fst args -- Do this instead of instanciate because then we take in the specified types right away
          args_with_initial_types = zip argnames inital_arg_types
          varenv_with_args = initial_varenv <> Map.fromList args_with_initial_types
          funenvWithMe = Map.insert name (Scheme argnames initial_return_type) inital_funenv

      -- Yes this is already checked in the caller of this ti but I don't want to add those decls to the env,
      (funvarsub, (funvardelcs_funenv, funvardelcs_varenv), typed_decls) <- checkProgram' (funenvWithMe, varenv_with_args) fundelcs

      -- Update the var and fun enviroment with the obtained information
      let varenv_args_and_vardecls = apply funvarsub funvardelcs_varenv <> varenv_with_args
          funenv_after_vardels = apply funvarsub funenvWithMe <> funvardelcs_funenv

      _ <- Debug.trace (blue ("Type checked decls got \n" ++ pretty typed_decls ++ "\nwith sub \n" ++ show funvarsub)) pure ()
      _ <- Debug.trace ("Checking body with fun env:\n" ++ prettyPrintMap funenvWithMe ++ "\nVar env:\n" ++ prettyPrintMap varenv_args_and_vardecls) pure ()

      let tc_env = env {getFunenv = funenv_after_vardels, getVarenv = varenv_args_and_vardecls, getMeta = meta, getFunName = Just name}
      (bodysub, retrurn_type_from_body) <- ti tc_env body

      -- Check if the infered return type of the body can be unified with the actual return type, look out for void types
      -- I am doing a special check here for Void so we can avoid it in unify, This still detects void unifications if they may happen later in another context that is not return checking!
      retsub <- case (retrurn_type_from_body, initial_return_type) of
            (VoidType, VoidType) -> pure nullSubst
            _ -> unify (apply bodysub tc_env) retrurn_type_from_body initial_return_type
                    `catchError` \str -> throwError $ red "Invalid return type in function '"++ blue name ++ red "'\n" ++ str

      -- Collect all the subs 
      let function_sub = bodysub `composeSubst` funvarsub `composeSubst` retsub

      -- Apply the function sub to the inital args to update them and apply it tot the return type 
      return (function_sub, FunType (apply function_sub inital_arg_types) (apply function_sub retrurn_type_from_body))

freshCounterStart :: TIState
freshCounterStart = 1

formatUnifyError :: Type -> TypecheckEnv  -> [Char]
formatUnifyError ty env = let
                          relevant = Map.keys $ Map.filter (ty ==) (getVarenv env)
                          meta = getMeta env in " at " ++ showStart meta ++ " until line " ++ (case endPos meta of SourcePos _ b a -> show (unPos b) ++ " column " ++ show (unPos a))
                                              ++ if null relevant then "" else "\nPotential variables of type " ++ show ty ++ " in scope include " ++ printWithCommas (typeListToStrings relevant)
                                              ++ ".\n\nLearn more about unification errors here: https://en.wikipedia.org/wiki/Unification_(computer_science) or here https://cloogle.org/#unify%20error"
                                      where typeListToStrings = map (\r -> '\'' : blue r ++ "'")


instance Types VarEnv where
  ftv env = foldMap ftv $ Map.elems env
  apply sub = Map.map (apply sub)

instance Types TypecheckEnv where
  ftv (TypecheckEnv funenv varenv  _ _) = ftv funenv <> ftv varenv
  apply sub (TypecheckEnv funenv varenv  meta name) = TypecheckEnv (apply sub funenv) (apply sub varenv) meta name

-- Ok so with apply with apply the substitutions to something!
-- todo We could also do this with a decl???

-- Checkprogram only adds the vardels and fundecls to the returend funenv and varenv, not the funvardecls ofcourse.

checkProgram :: (FunEnv, VarEnv) -> Program ReturnsCheckedP -> Either String (Subst, (FunEnv, VarEnv), Program TypecheckedP)
checkProgram env program = evalState (runExceptT $ checkProgram' env program) (length $ snd env)

checkProgram' :: (FunEnv, VarEnv) -> Program ReturnsCheckedP -> TI (Subst, (FunEnv, VarEnv), Program TypecheckedP)
checkProgram' env [] = return (nullSubst, env, [])
-- If they did not give a type then just infer it :)   todo we could also give it a type var and just call it again to go to the case with Just type, probably simpeler
checkProgram' (funenv, varenv) (VarDecl meta name Nothing expr : unchecked_program) = do
  let ti_env = TypecheckEnv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}
  (s1, ty) <- ti ti_env expr -- We could just set the type and go to the other case but then you do extra tc for no reason, we can always infer it cause no empty var types
  -- _ <- Debug.trace ("Inferred type of " ++ name ++ " is " ++ show ty) pure ()
  let varenv_with_me = Map.insert name ty varenv -- add the var to the env
      updated_var_env = apply s1 varenv_with_me -- Update the types (vars) in the var env with the substitution
      updated_funenv = apply s1 funenv -- Update the fun env just in case 
      env' = (updated_funenv, updated_var_env)
  -- Now we have type type of a var so lets move on
  (checked_program_sub, final_env, checked_program) <- checkProgram' env' unchecked_program -- Check the rest of the program given these variables  
  let sub = s1 `composeSubst` checked_program_sub 
  let var_decl_type = apply sub ty
  -- _ <- Debug.trace ("Checked program type var sub is " ++ show checked_program_sub) pure ()
  return (sub, apply s1 final_env, VarDecl meta name var_decl_type (upgrade expr):checked_program)
checkProgram' (funenv, varenv) (VarDecl meta name (Just specified_type) expr : unchecked_program) = do

  let tc_env = TypecheckEnv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}

  s1 <- tc tc_env expr specified_type

  let varenv_with_me = Map.insert name specified_type varenv -- add the var to the env
      updated_var_env = apply s1 varenv_with_me -- Update the types (vars) in the var env with the substitution
      updated_funenv = apply s1 funenv -- Update the fun env just in case 
      env' = (updated_funenv, updated_var_env)

  (checked_program_sub, final_env, checked_program) <- checkProgram' env' unchecked_program -- Merge with the result
  let sub = s1 `composeSubst` checked_program_sub 
  let var_decl_type = apply sub specified_type

  return (sub, apply s1 final_env, VarDecl meta name var_decl_type (upgrade expr) : checked_program)

checkProgram' env ((FunDecl meta name Nothing args fundecls body) : rest_program)  = do
                                                                                    newty <- newTyVar
                                                                                    checkProgram' env (FunDecl meta name (Just newty) args fundecls body : rest_program)
checkProgram' (initial_funenv, initial_varenv) (fun@(FunDecl meta name (Just _) args fundecls body) : rest_program)  = do

      -- Check for duplicate decls in the function, (only thing that ti does not do)
      let dupplicate_err = throwError . (++ (" in function '"++blue name++"' defined at " ++ showStart meta ++ ". "))
      _ <- either dupplicate_err pure (checkDuplicateDecls fundecls)

      -- Infer the type of the function, the ti does everything and does not need caller to add to the env, the sub will have all the information, this means you only need to just take the checked decls and apply the sub
      let ti_env = TypecheckEnv {getMeta = meta, getFunName = Just name,
                                getFunenv = initial_funenv,
                                getVarenv = initial_varenv}
      (funsub, funtype) <- ti ti_env fun
      -- Everything after this is just processing the result of the above ti

      _ <- Debug.trace ("Infered fuction " ++ blue name ++ " as " ++ show funtype )  pure ()
      (infered_arg_types, infered_retty) <- case funtype of
                                            FunType infered_args infered_retty -> pure (infered_args, infered_retty)
                                            infered_fun_ty -> throwError $ "Infered fuction '" ++ blue name ++ "' at " ++ showStart meta ++ " as type "  ++ show infered_fun_ty ++ " but this is not a function type so something went wrong."

      -- We only need the node, the subs comes from ti above
      (_, _, checked_funvardecls) <- checkProgram' (initial_funenv, initial_varenv) fundecls

      -- Add the fun decl to the fun env
      let argnames = map fst args
          funenvWithMe = apply funsub $ Map.insert name (Scheme argnames infered_retty) initial_funenv

                                                                        -- Do not propagate var env to hide local vars from the other functions
      (rest_sub, env, checked_program) <- checkProgram' (funenvWithMe, initial_varenv) rest_program
      -- Maybe we need to unify the function here with the function in the env'' and then add that to the sub

      -- Unify everything on the way back
      let sub = funsub `composeSubst` rest_sub
          updated_args = apply sub infered_arg_types
          updated_funvardecls = apply sub checked_funvardecls
          updated_retty = apply sub infered_retty
      -- _ <- Debug.trace ("Infered fuction " ++ blue name ++ " as " ++ show (apply sub funtype) ++ " after applying sub ("  ++ show sub ++ ") but we have" ++ show retty ++ " so apply s retty gives " ++ show ( apply sub retty)) pure ()

      return (sub, env, FunDecl meta name updated_retty (zip argnames updated_args) updated_funvardecls (map upgrade body) : checked_program)

instance Types (Decl TypecheckedP) where
      ftv (VarDecl _ _ ty _) = ftv ty
      ftv (FunDecl _ _ retty args funvars _) = ftv (map snd args) <> ftv funvars <> ftv retty
      apply sub (VarDecl meta name ty  expr) = VarDecl meta name (apply sub ty) expr
      apply sub (FunDecl meta name retty args funvars body) = FunDecl meta name (apply sub retty) (applyToArgs sub args) (apply sub funvars) body

applyToArgs :: Subst -> [(String, Type)] -> [(String, Type)]
applyToArgs s args = let names = map fst args
                         types = map snd args in zip names $ map (apply s) types

instance (Types typeable1, Types typeable2) => Types (typeable1, typeable2) where
      ftv (typeable1, typeable2) = ftv typeable1 <> ftv typeable2 -- This might be wrong because it merges both envs.
      apply sub (typeable1, typeable2) = (apply sub typeable1, apply sub typeable2)

getDeclType :: Decl TypecheckedP -> Type
getDeclType (VarDecl _ _ ty _) = ty
getDeclType (FunDecl _ _ retty args _ _) = FunType (map snd args) retty

getReturnType :: Decl TypecheckedP -> Type
getReturnType (VarDecl _ _ _ty _) = error "A var decl has no return type" -- But it can if you want to :) but I don't need that yet
getReturnType (FunDecl _ _ retty _ _ _) = retty

updateFunArgs :: [String] -> Map.Map String Type -> [(String, Type)]
updateFunArgs argnames function_sub =
  let maybe_types = map (\name -> (name, Map.lookup name function_sub)) argnames
    in map resolve_maybe maybe_types
  where
    resolve_maybe :: (String, Maybe Type) -> (String, Type)
    resolve_maybe (name, maybetype) =
      let err = error $ "Could not find variable with name '" ++ blue name ++ "' in update args map: " ++ show function_sub
          ty = fromMaybe err maybetype
        in (name, ty)

-- Insert arguments into the var env
insertFunArgsIntoEnv :: Map.Map String Type -> (String, Maybe Type) -> TI (Map.Map String Type)
insertFunArgsIntoEnv _env (name, maybeType) = do
  ty <- maybe newTyVar pure maybeType
  return $ Map.insert name ty _env

-- We need to add the return type to the enviroment, maybe do that earlier?
-- Then we need to add the function arguments to the variables map
-- These we then should put in the fun decl at the end after we get the substitution map
-- Make sure that they only exist with this function that takes a temperary env!
-- Try to unify the statements, I think we already have ti for lists of statements!

insertJust :: (Ord a) => Map.Map a b -> a -> Maybe b -> Map.Map a b
insertJust m key (Just b) = Map.insert key b m
insertJust m _ Nothing = m

-- Check duplicate declerations 
checkDuplicateDecls ::  Program ReturnsCheckedP -> Either String String
checkDuplicateDecls = checkDuplicateDecl' Map.empty Map.empty
  where
        checkDuplicateDecl' _ _ [] = Right "No duplicate declerations"
        checkDuplicateDecl' funmemory varmemory (FunDecl meta name _ _ _ _ : program) =
          case Map.lookup name funmemory of
            Nothing -> checkDuplicateDecl' (Map.insert name meta funmemory) varmemory program
            Just meta' -> Left $ red "Function with name '"
                          ++ blue name
                          ++ red "' is defined two times!\n"
                          ++ "The first time at: "
                          ++ showStart meta'
                          ++ " and the second time at: "
                          ++ showStart meta
        checkDuplicateDecl' funmemory varmemory (VarDecl meta name _ _: program) =
          case Map.lookup name varmemory of
            Nothing -> checkDuplicateDecl' funmemory (Map.insert name meta varmemory) program
            Just meta' -> Left $ red "Variable with name '"
                                ++ blue name
                                ++ red "' is defined two times!\n"
                                ++ "The first time at: "
                                ++ showStart meta'
                                ++ " and "
                                ++ "the second time at: "
                                ++ showEnd meta

-- Todo this should be another phase. We go from maybe type to type, but now fun decl is actually the same
--

varDeclTypeNotFoundError :: Show a1 => [Char] -> a1 -> a2
varDeclTypeNotFoundError name env = error $ "Type of " ++ blue name ++ " not found in type envrioment. This should not happen. It probably was never added to the variable enviroment " ++ show env

mergeTypesGlobalvars :: Program ReturnsCheckedP -> VarEnv -> Program ReturnsCheckedP
mergeTypesGlobalvars [] _ = []
mergeTypesGlobalvars (f@FunDecl {} : rest) env = f : mergeTypesGlobalvars rest env
mergeTypesGlobalvars ((VarDecl meta name _ expr) : rest) env =
  let justty = Map.lookup name env
  -- ty = fromMaybe (varDeclTypeNotFoundError name env) justty
   in VarDecl meta name justty expr : mergeTypesGlobalvars rest env

-- Todo this is basically the same function as the one above here. Bad? Maybe we should duplicate code for global var checks? Nah. Its just not needed and we have no time.
mergeTypesFunvars :: Program ReturnsCheckedP -> VarEnv -> Program TypecheckedP
mergeTypesFunvars [] _ = []
mergeTypesFunvars (FunDecl {} : _) _ = error "We do not support function definitions nested in functions yet"
mergeTypesFunvars ((VarDecl meta name _ expr) : rest) env =
  let ty = fromMaybe (Debug.trace "Error 101" varDeclTypeNotFoundError name env) $ Map.lookup name env
   in VarDecl meta name ty (upgrade expr) : mergeTypesFunvars rest env

-- todo Later this one shall have the buildins
--  Phase that we check that none of the user functions are named the default names!
defaultFunEnv :: Map.Map String Scheme
defaultFunEnv = Map.fromList [("print", Scheme ["input"] VoidType)]

defaultVarEnv :: Map.Map String Type
defaultVarEnv = Map.fromList []