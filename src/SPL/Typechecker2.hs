{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.Typechecker2 where


import SPL.AST
import SPL.Colors (blue, red, bold)
import SPL.Parser.SourceSpan (SourceSpan, endPos, showEnd, showStart)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Megaparsec (SourcePos (SourcePos), unPos)
import SPL.PrettyPrint (prettyPrintMap, printWithCommas)

data Scheme = Scheme [String] Type
  deriving (Show, Eq)

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
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a


data TypecheckEnv = TypecheckEnv { getFunenv :: FunEnv,
                                      getVarenv :: VarEnv,
                                      getMeta :: SourceSpan,
                                      getFunName :: Maybe String }

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
  ftv (FunType args decls rt) =  ftv args <> ftv rt <> ftv decls
  ftv (TypeVar var False) = Set.singleton var
  ftv _ = Set.empty -- No rigid!
  apply subst t@(TypeVar var False) = fromMaybe t (Map.lookup var subst)
  apply subst (TupleType t1 t2) = TupleType (apply subst t1) (apply subst t2)
  apply subst (ListType t) = ListType (apply subst t)
  apply subst (FunType args decls rt) = FunType (apply subst args) (apply subst decls) (apply subst rt)
  apply _ t = t

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

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
unify env (FunType at decls r) (FunType at' decls' r') = do
  -- We need to unify all the arguments from l with l' pair by pair
  let unify' = unify env
  s1 <- zipWithM unify' at at' -- same as: mapM (uncurry unify) (zip at at') -- No I would have never have known that witout the hint thing from vscode

  let s1' = foldr composeSubst nullSubst s1
  s2 <- zipWithM unify' decls decls'

  let s2' = foldr composeSubst nullSubst s2 -- fold all the argument subs together
  s3 <- unify env (apply s1' r) (apply s1' r')
  return $ s1' `composeSubst` s2' `composeSubst` s3
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
    s3 <- case t2 of
            -- If t2 is a list type we have to unify with the type inside the list
            ListType u1 -> unify env' t1 u1  `catchError` \err -> throwError $ "You tried to cons " ++ show t1 ++ " with " ++ show t2 ++ ", but this is not legal.\n" ++ err
            -- Else just try to unify and let it fail (most likely)
            _ -> unify env' t1 t2
    -- Todo ask is this needed?
    -- newTy <- newTyVar
    -- s4 <- tc env meta e2 (ListType newTy) 
    let s = s1 `composeSubst` s2 `composeSubst` s3 --`composeSubst` s4
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
  ti env (FunctionCallExpr meta funcName _args) = do
    let schemeM = Map.lookup funcName $ getFunenv env
    scheme <- case schemeM of
      Nothing -> throwError $ "Function " ++ funcName ++ " undefined. at " ++ showStart meta
      Just scheme -> return scheme
    -- If we get here we did find the function!
    retType <- instantiate scheme
    let retType' = case retType of
          FunType {} -> error $ "Got a funtype after instantiate scheme, how does that happen!!!!" ++ showStart meta
          a -> a
          {- FunType argsT rT -> do
            s1 <- zipWithM (tc env meta) args argsT
            let s = foldr composeSubst nullSubst s1
            return (s, apply s rT) -}
          -- _ -> error $ "Function environment contains something weird " ++ show retType ++ "!!!!!" ++ showStart meta
    return (nullSubst, retType')

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
                        (VoidType, VoidType) -> pure VoidType -- I think we have to have this case so that unify does not have to deal with VoidType
                        (retty1, retty2) -> (unify env retty1 retty2 `catchError` \err -> throwError $  red " Could NOT UNIFY RETURN TYPE OF STATEMENT BLOCK AT " ++ showStart (getMeta env) ++ err)
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
    let 
        funenv = getFunenv env'
        funname = case getFunName env' of 
                    Just name -> name 
                    Nothing -> "??? No function name, that means the return at "++ showStart meta ++" is outside a function?"
        maybe_return_type = Map.lookup funname funenv >>= \(Scheme _ return_type) -> pure return_type
        maybe_sub = unify env' expr_type <$> maybe_return_type
    s2 <- fromMaybe (throwError $ red "Invalid return type in function '" ++ blue funname ++ red "'"++" at " ++ showEnd meta ++ "\n") maybe_sub
    return (s1 `composeSubst` s2, expr_type)
            -- case getFunName env of
        -- Just name -> 
        -- Nothing -> throwError ("Return statement outside of function at " ++ showStart (getMeta env))
  -- Here we can check maybe the return type of the function?
  -- This has to be a funtion that checks the type of the return for the function we are in
  -- This should be in the env I think, just like meta
  ti _ (ReturnStmt _ Nothing) = return (nullSubst, VoidType)
  ti env (IfStmt meta cond consequent (Just alternative)) =
    do
      let env' = env' {getMeta = meta}
      s1 <- tc env cond BoolType
      (s2, ty1) <- ti env' consequent
      (s3, ty2) <- ti env' alternative
      s4 <- unify env' ty1 ty2
      let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
      return (s, ty1) -- we can do ty1 because of the previous returns checks, we know we can just pick one
  ti env (IfStmt meta cond consequent Nothing) =
    do
      let env' = env {getMeta = meta}
      s1 <- tc env' cond BoolType
      (s2, ty) <- ti env' consequent
      let s = s1 `composeSubst` s2
      return (s, ty)
  ti env (AssignStmt meta (Identifier var _) expr) = do
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
  -- Return the return type?
  ti env (FunDecl meta name retty args fundelcs body) = do
      let (funenv, varenv) = (getFunenv env, getVarenv env)
      -- Add the args to the varenv
      _ <- Debug.trace (blue ("Inferencing function '" ++ name ++ "': ")) pure ()
      -- _ <- Debug.trace ("CheckFunctions' got funenv: " ++ show funenv) pure ()
      -- _ <- Debug.trace ("Building arg env from args: " ++ show args) pure ()
      argenv <-  foldM insertFunArgsIntoEnv Map.empty args
      let varenvWithArgs = argenv <> varenv -- I hope this does not break :()
      return_type <- maybe newTyVar pure retty
      -- lets check the funvars
      -- Do this before adding the current function to the fun env. This prevents an infinite loop, todo add a better error message for this
      let dupplicate_err = throwError . (++ (" in function '"++blue name++"' defined at " ++ showStart meta ++ ". "))
      _ <- either dupplicate_err pure (checkDuplicateDecls fundelcs)
      _ <- Debug.trace ("Adding fundecl to varenv':\n" ++ prettyPrintMap varenvWithArgs) pure ()
      (funvarsub, (funenvAfterAddingVarDelcs,varenvWithArgsAndVarDecls)) <- buildVarEnv fundelcs (funenv, varenvWithArgs)
          -- Add a typescheme fun to the fun var
      let argnames = map fst args
          funenvWithMe = Map.insert name (Scheme argnames return_type) funenvAfterAddingVarDelcs
      _ <- Debug.trace ("Added fun to to funenv':\n" ++ prettyPrintMap funenvWithMe) pure ()

      -- These are the envs to use moving forward
      _ <- Debug.trace ("checking body with fun env:\n" ++ prettyPrintMap funenvWithMe ++ " var env:\n" ++ prettyPrintMap varenvWithArgsAndVarDecls) pure ()
      let final_env = env {getFunenv = funenvWithMe, getVarenv = varenvWithArgsAndVarDecls, getMeta = meta, getFunName = Just name}
      (bodysub, retrurn_type_from_body) <- ti final_env body
      -- Check if the infered return type of the body can be unified with the actual return type, todo look out for void types

      -- I am doing a special check here for Void so we can avoid it in unify, This still detects void unifications if they may happen later in another context that is not return checking!
      _ <- case (retrurn_type_from_body, return_type) of
            (VoidType, VoidType) -> pure nullSubst
            _ -> unify final_env retrurn_type_from_body return_type
                    `catchError` \str -> throwError $ red "Invalid return type in function '"++ blue name ++ red "'\n" ++ str

      let function_sub = bodysub `composeSubst` funvarsub `composeSubst` varenvWithArgsAndVarDecls
          args' = updateFunArgs argnames function_sub
          argTypes = map snd args' -- We need a list of types for the new fundecls 
      fundecls' <- mapM (varDeclsToType function_sub) fundelcs -- We do use the latest funenv I think
      return (function_sub, FunType argTypes fundecls' retrurn_type_from_body)

        where varDeclsToType :: VarEnv -> Decl ReturnsCheckedP -> TI Type
              varDeclsToType _ (FunDecl {}) = error "We don't support nested fundecls yet"
              varDeclsToType env' (VarDecl _ name' _ _) = case Map.lookup name' env' of
                                                          (Just ty) -> return ty
                                                          Nothing -> newTyVar

freshCounterStart :: TIState
freshCounterStart = 1

{- Checks the global variables and returns the variable env
If we duplicate buildVarEnv we could actually do this before the return type checking
 Again we use buildVarEnv for the global var decl but also in the function decl so we can not make the global vars a seperate phase without also needleesly duplicating code -}
checkGlobalVars :: Program ReturnsCheckedP -> Either String (Program ReturnsCheckedP, VarEnv)
checkGlobalVars p = do eitherResult <- evalState (runExceptT $ buildVarEnv p (Map.empty, Map.empty)) freshCounterStart
                       let eitherVarenv = snd . snd $ eitherResult
                       return $ merge eitherVarenv
                    where merge varenv = (mergeTypesGlobalvars p varenv, varenv)

{- This function builds the variables enviroment from a list of decls. So it only looks at the vardecl. 
During this we do type inference and type checking with the specified type. 
This one needs the fun env to resolve funcall expr-}
buildVarEnv :: Program ReturnsCheckedP -> (FunEnv, VarEnv) -> TI (Subst, (FunEnv, VarEnv))
buildVarEnv [] env = return (nullSubst, env)
-- Skip the fun envs
buildVarEnv (FunDecl {} : ds) env = buildVarEnv ds env
-- If they gave a type check it
buildVarEnv (VarDecl meta name (Just specified_type) expr : ds) (funenv, varenv) = do
  let env = TypecheckEnv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}
  s1 <- tc env expr specified_type

  (s2, env') <- buildVarEnv ds (funenv, Map.insert name specified_type varenv)
  return (s1 `composeSubst` s2, env')
-- If they did not give a type then just infer it :)
buildVarEnv (VarDecl meta name Nothing expr : ds) (funenv, varenv) = do
  let env = TypecheckEnv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}
  (s1, ty) <- ti env expr
  (s2, env') <- buildVarEnv ds (funenv, Map.insert name ty varenv)
  return (s1 `composeSubst` s2, env')

formatUnifyError :: Type -> TypecheckEnv  -> [Char]
formatUnifyError ty env = let
                          relevant = Map.keys $ Map.filter (ty ==) (getVarenv env)
                          meta = getMeta env in " at " ++ showStart meta ++ " until line " ++ (case endPos meta of SourcePos _ b a -> show (unPos b) ++ " column " ++ show (unPos a))
                                              ++ if null relevant then "" else "\nPotential variables of type " ++ show ty ++ " in scope include " ++ printWithCommas (typeListToStrings relevant)
                                              ++ ".\n\nLearn more about unification errors here: https://en.wikipedia.org/wiki/Unification_(computer_science) or here https://cloogle.org/#unify%20error"
                                      where typeListToStrings = map (\r -> '\'' : blue r ++ "'")

-- todo maybe this can be the ti instance for decl! But probably not cause we need to get TypecheckedP
checkFunctions :: (FunEnv, VarEnv) -> Program ReturnsCheckedP-> Either String (Subst, (FunEnv, VarEnv), Program TypecheckedP)
checkFunctions env program = evalState (runExceptT $ checkFunctions' program env) (length $ snd env)
  where
    checkFunctions' :: Program ReturnsCheckedP -> (FunEnv, VarEnv) -> TI (Subst, (FunEnv, VarEnv), Program TypecheckedP)
    checkFunctions' [] _ = return (nullSubst, env, [])
    -- VarDecl, we alrady checked these earlier in order to hoist them up top
    -- We still need to upgrade them though to typecheckedp
    -- So this does not do anything
    checkFunctions' (VarDecl info name maybetype d : rest) env'@(_, varenv) = do
      (sub, env'', program') <- checkFunctions' rest env'
      let ty = varDeclTypeNotFoundError name varenv `fromMaybe` maybetype -- This should always be Just because its a global var
          typedD = upgrade d
      return (sub, env'', VarDecl info name ty typedD : program')
    -- FunDecl, FunDeclT ParsedP = Maybe Type
    checkFunctions' (fun@(FunDecl meta name _ args _ _) : rest_program) (funenv, varenv) = do
      -- if we get a pattern match error its probably from here?
      (_, funtype) <- ti TypecheckEnv {getMeta = meta, getFunName = Just name, getFunenv = funenv, getVarenv = varenv} fun
      let new_fundecl = updateDeclType funtype fun
          argnames = map fst args
          actual_retty = getReturnType new_fundecl
          -- Also we have to add the type scheme to the enviroment here again I think, makes sense because we forget everything else
          funenvWithMe = Map.insert name (Scheme argnames actual_retty) funenv

      (sub, env'', checked_program) <- checkFunctions' rest_program (funenvWithMe, varenv)
      -- I think we maybe now can look up the return type of this function in env
      -- Idk if we have to compose these subs here??
        -- We do use the latest funenv I think
      return (sub, env'', new_fundecl : checked_program)
      -- Update the tree node with the FunType!

-- todo make this a where ?
updateDeclType :: Type -> Decl ReturnsCheckedP -> Decl TypecheckedP
updateDeclType ty (VarDecl meta name _ e) = VarDecl meta name ty (upgrade e)
updateDeclType (FunType arg_types decl_types retty') (FunDecl meta name _ args fundecls body) =
  let new_fundecls = zipWith updateDeclType decl_types fundecls
      new_args = [(argname, arg_type) | ((argname ,_), arg_type) <- zip args arg_types]
    in FunDecl meta name retty' new_args new_fundecls (map upgrade body)
updateDeclType ty fun@(FunDecl {}) = error $ "Can only update function decls with function types. Instead you tried to update " ++ show fun ++ " with a " ++ show ty
    -- how do we make a type scheme?

getDeclType :: Decl TypecheckedP -> Type
getDeclType (VarDecl _ _ ty _) = ty
getDeclType (FunDecl _ _ retty args fundecls _) = FunType (map snd args) (map getDeclType fundecls) retty

getReturnType :: Decl TypecheckedP -> Type
getReturnType (VarDecl _ _ _ty _) = error "A var decl has no return type" -- But it can if you want to :) but I don't need that yet
getReturnType (FunDecl _ _ retty _ _ _) = retty

updateFunArgs :: [String] -> Map.Map String Type -> [(String, FunDeclT TypecheckedP)]
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
                          ++ "cand the second time at: "
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
mergeTypesGlobalvars (f@(FunDecl {}) : rest) env = f : mergeTypesGlobalvars rest env
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
