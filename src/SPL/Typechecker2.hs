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
import Data.Map (Map)

data Scheme = Scheme (Set.Set String) Type
  deriving (Show, Eq)

type FunEnv = Map String Scheme

type VarEnv = Map String Type

{- A subst is a map from type var names to types-}
type Subst = Map String Type

type FreeTypeVarNames = Set.Set String

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

type TIState = TIenv

type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) emptyTypecheckEnv

evalTI :: TI a -> Either String a -- Like run but we throw away the state
evalTI t = fst $ runTI t

initTyCounterStart :: Int
initTyCounterStart = 1

--- Type inference enviroment
data TIenv = TIenv {getFunenv :: FunEnv,
                    getVarenv :: VarEnv,
                    getMeta :: SourceSpan,
                    getFunName :: Maybe String,
                    getFunArgNames :: Maybe [String],
                    getTypeVarCounter :: Int
                  }
    deriving Show



{- Functions for the TIenv state monad-}

-- Meta is not defined here but maybe if you don't touch it its ok?
emptyTypecheckEnv :: TIenv
emptyTypecheckEnv = TIenv { getFunenv = defaultFunEnv, getVarenv = defaultVarEnv, getMeta = undefined, getFunArgNames = Nothing, getTypeVarCounter = initTyCounterStart, getFunName = Nothing}

increaseTypeVarCounter :: TI ()
increaseTypeVarCounter = modify $ \env@(TIenv {getTypeVarCounter = count}) -> env {getTypeVarCounter = count + 1}

-- Function that gets a var from the var env or throw a var not found if it can't
lookupVarType :: String -> TI Type
lookupVarType varname = do
  varenv <- gets getVarenv
  case Map.lookup varname varenv of
    Just varT -> return varT
    Nothing -> gets getMeta >>= \meta -> throwError $ "Undefined variable " ++ show varname ++ " at " ++ show meta ++ "."

lookupFunScheme :: String -> TI Scheme
lookupFunScheme name = do
  funenv <- gets getFunenv
  case Map.lookup name funenv of
    Nothing -> gets getMeta >>= \meta -> throwError $ "Function '" ++  blue name ++ "' is undefined. at " ++ showStart meta
    Just scheme -> return scheme

lookupFunType :: String -> TI Type
lookupFunType name = do
  scheme <- lookupFunScheme name
  instantiate scheme
-- Here we don't check if its a fun type. Too happy about no return function

lookupFunName :: TI String
lookupFunName = do
  name <- gets getFunName
  case name of
    Nothing -> gets getMeta >>= \meta -> throwError $ red "No current function name in type inference envrioment. " ++ "This is a compiler error. I don't know the name of the function I am currently checking at " ++ showStart meta
    Just found -> return found

lookupArgNames :: TI [String]
lookupArgNames = do
  names <- gets getFunArgNames
  case names of
    Nothing -> gets getMeta >>= \meta -> throwError $ red "No current function arg names in type inference envrioment. " ++ "This is a compiler error. I don't know the argument name of the function I am currently checking at " ++ showStart meta
    Just found -> return found

lookupCurrentFunScheme :: TI Scheme
lookupCurrentFunScheme = do
  funname <- lookupFunName `catchError` \err -> throwError $ "While looking type scheme of function currently being checked got error looking up the name of the current function. Here is that error: \n" ++ err
  lookupFunScheme funname

applySubToTIenv :: Subst -> TI ()
applySubToTIenv sub = modify (apply sub)

replaceMeta :: SourceSpan -> TI ()
replaceMeta meta = modify (\env -> env {getMeta = meta})

replaceFunName :: String -> TI ()
replaceFunName name = modify (\env -> env {getFunName = Just name})

removeFunName :: TI ()
removeFunName = modify (\env -> env {getFunName = Nothing})

setFunArgNames :: [String] -> TI ()
setFunArgNames names = modify (\env -> env {getFunArgNames = Just names})

removeFunArgNames :: TI ()
removeFunArgNames = modify (\env -> env {getFunArgNames = Nothing})

replaceFunArgNames :: [String] -> TI ()
replaceFunArgNames names = modify (\env -> env {getFunArgNames = Just names})

newTyVar :: TI Type
newTyVar = do
  s <- gets getTypeVarCounter
  increaseTypeVarCounter
  let name = toTyVar s
  return $ Debug.trace ("Making type var " ++ show name) TypeVar (reverse name) False
  where
    toTyVar c
      | c < 26 = [toEnum (97 + c)]
      | otherwise = let (n, r) = c `divMod` 26 in toEnum (97 + r) : toTyVar (n - 1)

extendVarEnv :: VarEnv -> TI ()
extendVarEnv new_varenv = modify (\env@TIenv {getVarenv = varenv} -> env {getVarenv = varenv <> new_varenv})

extendFunEnv :: String -> Scheme -> TI ()
extendFunEnv funname fun_type = modify (\env@TIenv {getFunenv = funenv} -> env {getFunenv = Map.insert funname fun_type funenv })

resetVarEnv :: VarEnv -> TI ()
resetVarEnv varenv = modify (\env -> env {getVarenv = varenv})

{- End of Functions for the TIenv state -}

class Types a where
  ftv :: a -> FreeTypeVarNames
  apply :: Subst -> a -> a


class Typecheck a where
  {-# MINIMAL tc | ti #-}
  tc :: a -> Type -> TI Subst
  tc a t = do
    (s1, inferredT) <- ti a
    s2 <- unify t inferredT
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return $ s
  ti :: a -> TI (Subst, Type)
  ti a = do
    t <- newTyVar
    s <- tc a t
    applySubToTIenv s
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
  ftv (Scheme vars t) = ftv t `Set.difference` vars -- Only return the free variables from t that are in vars, this way you can do rigid without the boolean. Todo, ask, vraag
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t) -- make a new scheme by applying the sub, todo idk why the delete though


instance (Types a) => Types [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty


-- The instantiation function replaces all bound type variables in
--  a type scheme with fresh type variables.
-- So with this you can make an instance of a type scheme
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- mapM (const newTyVar) (Set.toList vars) -- newTyVar for each var in the scheme
  let s = Map.fromList (zip (Set.toList vars) nvars) -- todo make this nice r
   in return $ apply s t

varBind :: SourceSpan -> String -> Type -> TI Subst
varBind meta u t
  | u `Set.member` ftv t =
      throwError $
        "Occurs check: cannot construct the infinite type " ++ u ++ " ~ " ++ show t ++ " at " ++ showStart meta
  | otherwise = return (Map.singleton u t)

-- todo, we could also include variables names here maybe
-- Based on the variables in the varenv we could do suggestions of types to use!!
unify :: Type -> Type -> TI Subst -- Are 2 types be the same?
unify (FunType args r) (FunType args' r') = do
  -- We need to unify all the arguments from l with l' pair by pair
  s1 <- zipWithM unify args args' -- same as: mapM (uncurry unify) (zip at at') -- No I would have never have known that witout the hint thing from vscode
  let s1' = foldr composeSubst nullSubst s1
  s2 <- unify (apply s1' r) (apply s1' r')
  return $ s1' `composeSubst` s2
unify (TypeVar u False) t = gets getMeta >>= \meta -> varBind meta u t -- bind the type var!
unify t (TypeVar u False) = gets getMeta >>= \meta -> varBind meta u t
unify ty1@(TypeVar name1 True) ty2@(TypeVar name2 True) = if name1 == name2 then return nullSubst else unifyError ty1 ty2
unify ty1@(TypeVar _ True) ty2 = unifyError ty1 ty2
unify ty1 ty2@(TypeVar _ True) = unifyError ty1 ty2
unify (ListType t1) (ListType t2) = unify t1 t2
unify (TupleType t1 t2) (TupleType t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  let s = s1 `composeSubst` s2
  applySubToTIenv s
  return $ s
unify IntType IntType = return nullSubst
unify BoolType BoolType = return nullSubst
unify CharType CharType = return nullSubst
-- todo probably add case for Void Void 
unify t1 t2 = unifyError t1 t2

instance Typecheck (Literal ReturnsCheckedP) where
  ti (IntLit _) = return (nullSubst, IntType)
  ti TrueLit = return (nullSubst, BoolType)
  ti FalseLit = return (nullSubst, BoolType)
  ti (CharLit _) = return (nullSubst, CharType)
  ti EmptyListLit = do
    var <- newTyVar
    return (nullSubst, ListType var) -- ListType with typevar inside!
  ti (TupleLit (e1, e2)) = do
    (s1, t1) <- ti e1
    (s2, t2) <- ti e2
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, TupleType t1 t2)

-- Check if the type of 2 expr can be CheckType and then return ResultType
--   ResultType -> CheckType -> ...
tcBinOp :: Type -> Type -> Expr ReturnsCheckedP  -> Expr ReturnsCheckedP  -> TI (Subst, Type)
tcBinOp resultT checkT e1 e2 = do
  s1 <- tc e1 checkT
  s2 <- tc e2 checkT
  let s = s1 `composeSubst` s2
  applySubToTIenv s
  return (s, resultT)

-- todo Dependent types for booleans

{- Check if the infered types of 2 expr can be unified with eachother and then return ResultType as the type of the 2 expressions
We are checking if the two types of the expressions are equal in a sense -}
tcBinOpEqual :: Type -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpEqual resultT e1 e2 = do
  (s1, t1) <- ti e1
  (s2, t2) <- ti e2
  s3 <- unify t1 t2
  let s = s1 `composeSubst` s2 `composeSubst` s3
  applySubToTIenv s
  return (s, apply s resultT)

{- Checks if the types of 2 expressions match the given type and returns the given type as the type of the expression -}
tcBinOpIdentity :: Type -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpIdentity t = tcBinOp t t

-- Checks if the types of 2 expressions match the given type returns and returns a boolType
tcBinOpBoolean :: Type -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP-> TI (Subst, Type)
tcBinOpBoolean = tcBinOp BoolType


-- First tries to infer as boolean with 2 ints and then with 2 chars if both fail an error is thrown using throwError
tcCharIntToBoolOverload :: BinOp -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcCharIntToBoolOverload op e1 e2 = tcBinOpBoolean IntType e1 e2 `catchError`
                                \_ -> tcBinOpBoolean CharType e1 e2  `catchError`
                                  \_ -> do
                                    (_, t1) <- ti e1
                                    (_, t2) <- ti e2
                                    meta <- gets getMeta
                                    errorTail <- unifyError t1 t2
                                    throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to " ++ pretty op ++" should be either Int or Char but you gave " ++ show t1 ++ show op ++ show t2 ++ errorTail

instance Typecheck (Expr ReturnsCheckedP) where
  ti (LiteralExpr meta lit) = replaceMeta meta >> ti lit
  ti (BinOpExpr meta Mul e1 e2) = replaceMeta meta >> tcBinOpIdentity IntType e1 e2
  ti (BinOpExpr meta Mod e1 e2) = replaceMeta meta >> tcBinOpIdentity IntType e1 e2
  ti (BinOpExpr meta Add e1 e2) = replaceMeta meta >> tcBinOpIdentity IntType e1 e2
  ti (BinOpExpr meta Div e1 e2) = replaceMeta meta >> tcBinOpIdentity IntType e1 e2
  ti (BinOpExpr meta Sub e1 e2) = replaceMeta meta >> tcBinOpIdentity IntType e1 e2
  ti (BinOpExpr meta Neq e1 e2) = replaceMeta meta >> tcBinOpEqual BoolType e1 e2
  ti (BinOpExpr meta And e1 e2) = replaceMeta meta >> tcBinOpIdentity BoolType e1 e2
  ti (BinOpExpr meta Gte e1 e2) = replaceMeta meta >> tcCharIntToBoolOverload Gte e1 e2
  ti (BinOpExpr meta Lte e1 e2) = replaceMeta meta >> tcCharIntToBoolOverload Lte e1 e2
  ti (BinOpExpr meta Gt e1 e2) = replaceMeta meta >> tcCharIntToBoolOverload Gt e1 e2
  ti (BinOpExpr meta Lt e1 e2) = replaceMeta meta >> tcCharIntToBoolOverload Lt e1 e2
  ti (BinOpExpr meta Eq e1 e2) = replaceMeta meta >> tcBinOpEqual BoolType e1 e2
  ti (BinOpExpr meta Or e1 e2) = replaceMeta meta >> tcBinOpIdentity BoolType e1 e2
  ti (BinOpExpr meta Cons e1 e2) = do
    replaceMeta meta
    (s1, t1) <- ti e1
    (s2, listty) <- ti e2
    -- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++  " s2: " ++ show s2 ) pure ()
    s3 <- case listty of -- If t2 is a list type we have to unify with the type inside the list
            ListType u1 -> unify t1 u1  `catchError` \err -> throwError $ "You tried to cons " ++ show t1 ++ " with " ++ show listty ++ ", but this is not legal.\n" ++ err
            _ -> unify t1 listty -- Else just try to unify and let it fail (most likely)
    let s = s1 `composeSubst` s2 `composeSubst` s3
    -- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++ show s4 ++ " s4!" ++ "s: " ++ show s ) pure ()
    applySubToTIenv s
    return (s, apply s $ ListType t1) -- This is good because we always get a list type like this and we are sure we can put t1 inside it 
  ti (UnaryOpExpr meta Negate e) = do
    replaceMeta meta
    (s1, t) <- ti e
    s2 <- unify BoolType t -- Check if its a bool, again here we could actually negate the bool maybe, like dependent types but only for bools?
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, apply s t)
  ti (UnaryOpExpr meta (FieldAccess field) _expr) = replaceMeta meta >> error "Field access is not working yet :["
  -- Todo Variables can have fields but I am just going to take the type of the String, I am leaving the warning
  ti (VariableExpr meta (Identifier var field)) = replaceMeta meta >> lookupVarType var >>= \sigma -> pure (nullSubst, sigma)
  ti (FunctionCallExpr meta "print" args) = replaceMeta meta >> pure (nullSubst, VoidType) -- Print is special just return Void, but ok we should do another found where we infer and replace expr, but that is just a checkProgram on stmt and decl!!!!!!
  ti (FunctionCallExpr meta funcName args) = do
    replaceMeta meta
    scheme <- lookupFunScheme funcName
    funtype <- instantiate scheme
    (funargs, retty) <- case funtype of
          FunType funargs retty -> Debug.trace (black "Instancitated " ++ funcName ++ " as " ++ show funtype ) pure (funargs, retty)
          ty -> error $ ("Did not get a funtype after instantiate scheme, how does that happen!!" ++ show ty ++ " ") ++ showStart meta

    let arg_count = length funargs
        n_given_args = length args
    when (n_given_args > arg_count) (throwError $ red "You called the '"++ blue funcName ++ red "' function at " ++ showStart meta ++ red " with too many arguments " ++ "("++ green ( show n_given_args )++ " > " ++ green (show arg_count) ++ ")" ++ "\nThe '"++ blue funcName ++ "' function takes " ++ green ( show arg_count) ++ " argument" ++ (if arg_count == 1 then "" else "s") ++ " but you gave " ++ green ( show n_given_args) ++ " argument" ++ (if n_given_args == 1 then "" else "s") ++".")
    when (n_given_args < arg_count) (throwMissingArgs $ arg_count - n_given_args)

    f <- zipWithM tc args funargs
    let sub = foldr composeSubst nullSubst f

    applySubToTIenv sub

    return (sub, apply sub retty)
      where
        throwMissingArgs :: Int -> TI ()
        throwMissingArgs missing = do
          funcname <- lookupFunName
          names <- if funcname == funcName then lookupArgNames >>= \argnames -> pure $ "\nThe missing arguments are called " ++ (intercalate " and " (map blue (drop missing argnames)) ++ ".") else pure ""
          throwError $ red "Function call at " ++ showStart meta ++ red " is missing " ++ green (show missing) ++ red " arguments." ++ names
          return undefined


instance Typecheck (Stmt ReturnsCheckedP) where
  ti (ReturnStmt meta (Just expr)) = do
    replaceMeta meta
    scheme <- lookupCurrentFunScheme `catchError` \err -> throwError $ err ++ "\nNo function name, that means the return at "++ showStart meta ++ red " is outside a function? Don't do that."

    funtype <- instantiate scheme
    (_, returntype) <- case funtype of -- don't generalize this one I think, otherwise you still don't really know if you have retty and arg types
                                  FunType argtypes returntype -> pure (argtypes,returntype)
                                  t -> throwError $ "Function env returned non function type " ++ show t

    s1 <- tc expr returntype `catchError` \err -> lookupFunName >>= \funname -> throwError $ red "Invalid return type in function '" ++ blue funname ++ red "'" ++ " at " ++ showEnd meta ++ "\n" ++ err
    applySubToTIenv s1 -- Apply s1 on function scheme!
    -- todo Now either get the return type again from the env or 
    return (s1, returntype)
  ti (ReturnStmt _ Nothing) = return (nullSubst, VoidType)
  ti (IfStmt meta cond consequent (Just alternative)) = do
    replaceMeta meta
    s1 <- tc cond BoolType
    (s2, ty1) <- ti consequent
    (s3, ty2) <- ti alternative
    s4 <- unify ty1 ty2
    let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
    applySubToTIenv s
    return (s, apply s ty1) -- we can do ty1 because of the previous returns checks, we know we can just pick one
  ti (IfStmt meta cond consequent Nothing) = do
    replaceMeta meta
    s1 <- tc cond BoolType
    (s2, ty) <- ti consequent
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, apply s ty)
  -- todo add fields
  ti (AssignStmt meta (Identifier var fiels) expr) = do
    replaceMeta meta
    (s1, t) <- ti expr
    varT <- lookupVarType var
    s2 <- unify varT t
    let s  = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, VoidType) -- This is void type because its never a return
  ti (WhileStmt meta cond stmts) = do
    replaceMeta meta
    s1 <- tc cond BoolType
    (s2, ty) <- ti stmts
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, apply s ty)
  -- todo this one idk, maybe we should not apply it
  ti (ExprStmt meta e) = replaceMeta meta >> ti e >>= \(sup, _) -> applySubToTIenv sup >> pure (sup, VoidType) -- Also void type because its not a return
  -- ti env meta (BlockStmt stmt) = Debug.trace "ti called on BlackStmt!! That should not happen right?" ti env meta stmt
  ti (BlockStmt _body) = error "ti called on BlackStmt!! That should not happen right during type checking?" -- but when we do stop at return?


-- Maybe we also do this for list of expressions but probably not because a list of expressions does not make sense
instance Typecheck [Stmt ReturnsCheckedP] where

  {- todo this is wrong because it does not update the envrioment every time but uses the one from at the start! 
       I guess thats ok for statement and below because they can't change env. But lets just make this one only for statements -}
  ti [] = return (nullSubst, VoidType)
  ti [stmt] = ti stmt -- This fixes that if you have a return statement as the last statement that the last will be compared with void because [] = VoidType
  ti (stmt:later) = do
    (mysub, mytype) <- ti stmt
    (latersub, othertype) <- ti later
    let sub = mysub `composeSubst` latersub
    case (mytype, othertype) of
      -- I think we have to have these cases so that unify does not have to deal with VoidType
      (VoidType, VoidType) -> pure (nullSubst, VoidType)
      -- If retty1 is not Void and the rest is void it means that I return something! In that case just propagete what I return, todo check this 
      (retty1, VoidType) -> pure (nullSubst, retty1)
      -- If retty1 is Void and the rest is not void it means that I do not return something but later we do! In that case just propagete what we return later
      (VoidType, retty2) -> pure (nullSubst, retty2) -- I think we have to have this case so that unify does not have to deal with VoidType
      (retty1, retty2) -> do
                            retsub <- unify retty1 retty2 `catchError` \err -> throwError $  red " Could not unify return types of statement list.\n" ++ err
                            let subsub = retsub `composeSubst` sub
                            pure (subsub, apply subsub retty2)



-- todo remove some of the debug.trace


{-

The problem with this is that we don't need to actually combine the subs from these but if one is wrong we need to know it.
  OHHHH We return a fun type ofcourse!! From that we can upgrade the decl in the wrapper function!

-}
instance Typecheck (Decl ReturnsCheckedP) where
  ti (VarDecl meta _ Nothing expr) = replaceMeta meta >> ti expr >>= \(sub, ty) -> applySubToTIenv sub >> pure (sub, ty)
  ti (VarDecl meta _ (Just ty) expr) = replaceMeta meta >> tc expr ty >>= \sub -> applySubToTIenv sub >> pure (sub, ty)
  ti (FunDecl meta name retty args fundelcs body) = do -- The returned sub contains the information of the args, decls and body, the caller does not need to add anything to the env
      Debug.trace (blue ("Inferencing function '" ++ name ++ "': ")) pure () -- this was as tight as I could make it

      initial_vars <- gets getVarenv -- Save the current vars 

      replaceMeta meta -- Set error generation information
      replaceFunName name

      inital_arg_types <- mapM (maybe newTyVar pure . snd) args -- Get the args 
      initial_retty <- maybe newTyVar pure retty -- Get the return type         <- First two things to make the type scheme

      let free_vars = ftv inital_arg_types <> ftv initial_retty  -- Make the type scheme
          fun_type = FunType inital_arg_types initial_retty
          type_scheme = Scheme free_vars fun_type
      extendFunEnv name type_scheme 

      let argnames = map fst args  -- Extend var env with the arguments
          named_and_typed_args = Map.fromList $ zip argnames inital_arg_types
      replaceFunArgNames argnames -- for error messages
      extendVarEnv named_and_typed_args

      fundelc_inference <- mapM ti fundelcs -- Extend var env with the arguments
      let infered_fundelc_types = map snd fundelc_inference
          named_and_typed_fundecls = Map.fromList $ zip declnames infered_fundelc_types
          declnames = map getDeclName fundelcs 
      extendVarEnv named_and_typed_fundecls
      
      let infered_fundelc_subs = map fst fundelc_inference -- Create the substitution for the decls
          infered_fundelc_sub = foldr composeSubst nullSubst infered_fundelc_subs

      infered_body_sub <- tc body initial_retty 

      let function_sub = infered_fundelc_sub `composeSubst` infered_body_sub
      
      applySubToTIenv function_sub

      removeFunName
      removeFunArgNames
      resetVarEnv initial_vars

      return (function_sub, apply function_sub  fun_type)


getRelevantVars :: Type -> TI String
getRelevantVars ty = do
   varEnv <- gets getVarenv
   let relevant = Map.keys $ Map.filter (ty ==) varEnv in
      if null relevant  then pure "" else pure $ "\nPotential variables of type " ++ show ty ++ " in scope include " ++ printWithCommas (typeListToStrings relevant)
  where typeListToStrings = map (\r -> '\'' : blue r ++ "'")

unifyError ty1@(TypeVar name1 True) ty2@(TypeVar name2 True) = do
  errorTail <- unifyErrorTail ty1 ty2
  throwError $  "Cannot unify " ++ show ty1 ++ " with " ++ show ty2 ++ " as '" ++ blue name1 ++ "' and '" ++ blue name2 ++ "' are two " ++ bold "different" ++ " rigid type variable because they have a different name.\nA rigid type variable means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty2@(TypeVar u True) ty1 = do
  errorTail <- unifyErrorTail ty1 ty2
  throwError $ "Cannot unify " ++ show ty1 ++ " with " ++ show ty2 ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty1 ty2@(TypeVar u True) = do
  errorTail <- unifyErrorTail ty1 ty2
  throwError $ "Types do not unify:\n" ++ show ty1 ++ " vs. " ++ show ty2 ++ "Cannot unify " ++ show ty2 ++ " with " ++ show ty1 ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty1 ty2 = unifyErrorTail ty1 ty2 >>= \errorTail -> throwError $ "Types do not unify:\n" ++ show ty1 ++ " vs. " ++ show ty2 ++ errorTail

unifyErrorTail :: Type -> Type -> TI String
unifyErrorTail ty1 ty2 = do
  relevant1 <- getRelevantVars ty1
  relevant2 <- getRelevantVars ty2
  meta <- gets getMeta
  return $ " at " ++ showStart meta ++ " until line " ++ (case endPos meta of SourcePos _ b a -> show (unPos b) ++ " column " ++ show (unPos a)) ++ relevant1 ++ relevant2
            ++ ".\n\nlearn more about unification errors here: https://en.wikipedia.org/wiki/unification_(computer_science) or here https://cloogle.org/#unify%20error"


instance Types VarEnv where
  ftv env = ftv $ Map.elems env
  apply sub = Map.map (apply sub)

instance Types FunEnv where
  ftv env = ftv $ Map.elems env
  apply sub = Map.map (apply sub)

-- If its a partial thing it should be between brackets 

instance Types TIenv where
  ftv (TIenv funenv varenv _ _ _ _) = ftv funenv <> ftv varenv
  apply sub (TIenv funenv varenv  meta name argnames counter) = TIenv (apply sub funenv) (apply sub varenv) meta name argnames counter

-- Ok so with apply with apply the substitutions to something!
-- todo We could also do this with a decl???

-- Checkprogram only adds the vardels and fundecls to the returend funenv and varenv, not the funvardecls ofcourse.

-- checkProgram :: Program ReturnsCheckedP -> Either String (Program TypecheckedP)
checkProgram :: Program 'ReturnsCheckedP -> (Either String (Program 'TypecheckedP), TIState)
checkProgram program = runTI (checkDecls program)

checkDecls :: Program ReturnsCheckedP -> TI (Program TypecheckedP)
checkDecls [] = return []
checkDecls ( vardecl@(VarDecl meta name Nothing expr):later) = do
  future <- checkDecls later
  (_,ty) <- ti vardecl
  return $ VarDecl meta name ty (upgrade expr) : future 
checkDecls ( vardecl@(VarDecl meta name (Just _) expr):later) = do
  (_,ty) <- ti vardecl
  future <- checkDecls later
  return $ VarDecl meta name ty (upgrade expr) : future -- This expre would be trivial to upgrade cause check expr we have the type here
checkDecls (fundecl@(FunDecl meta name _ args fundecls body):later) = do
  checked_fundecls <- checkDecls fundecls 

  (_, fun_type) <- ti fundecl 
  (infered_argument_types, infered_return_type) <- case fun_type of 
      (FunType argty returnty)-> pure (argty, returnty)
      _ -> error (red "Ti on fundecl did not retun funtype at" ++ showStart meta)
  let argsnames = map fst args 
      typed_args = zip argsnames infered_argument_types
  future <- checkDecls later
  return $ FunDecl meta name infered_return_type typed_args checked_fundecls (map upgrade body) : future -- This expre would be trivial to upgrade cause check expr we have the type here




{-

-- If they did not give a type then just infer it :)   todo we could also give it a type var and just call it again to go to the case with Just type, probably simpeler
checkProgram' (funenv, varenv) (VarDecl meta name Nothing expr : unchecked_program) = do
  let ti_env = TIenv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}
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

  let tc_env = TIenv {getMeta = meta, getFunName = Nothing, getFunenv = funenv, getVarenv = varenv}

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
-- FUNDELC                                                                                   
checkProgram' (initial_funenv, initial_varenv) (fun@(FunDecl meta name (Just retty) args fundecls body) : rest_program)  = do

      -- Check for duplicate decls in the function, (only thing that ti does not do)
      let dupplicate_err = throwError . (++ (" in function '" ++ blue name ++ "' defined at " ++ showStart meta ++ ". "))
      _ <- either dupplicate_err pure (checkDuplicateDecls fundecls)

      -- Add fun scheme to the fun env
      inital_arg_types <- mapM (maybe newTyVar pure . snd) args
      let
          inital_fun_type = FunType inital_arg_types retty
          inital_fv = ftv inital_fun_type
          scheme = Scheme inital_fv inital_fun_type
          funenvWithMe = Map.insert name scheme initial_funenv


      -- Infer the type of the function, the ti does everything and does not need caller to add to the env, the sub will have all the information, this means you only need to just take the checked decls and apply the sub
      let ti_env = TIenv {getMeta = meta, getFunName = Just name,
                                getFunenv = funenvWithMe,
                                getVarenv = initial_varenv}
      (funsub, funtype) <- ti ti_env fun
      -- Everything after this is just processing the result of the above ti

      _ <- Debug.trace ("Infered fuction " ++ blue name ++ " as " ++ show funtype )  pure ()
      (infered_arg_types, infered_retty) <- case funtype of
                                            FunType infered_args infered_retty -> pure (infered_args, infered_retty)
                                            infered_fun_ty -> throwError $ "Infered fuction '" ++ blue name ++ "' at " ++ showStart meta ++ " as type "  ++ show infered_fun_ty ++ " but this is not a function type so something went wrong."

      -- We only need the nodes, the subs comes from ti above
      (_, _, checked_funvardecls) <- checkProgram' (initial_funenv, initial_varenv) fundecls

                                                                        -- Do not propagate var env to hide local vars from the other functions
      (rest_sub, env, checked_program) <- checkProgram' (funenvWithMe, initial_varenv) rest_program
      -- Maybe we need to unify the function here with the function in the env'' and then add that to the sub

      -- Unify everything on the way back
      let sub = funsub `composeSubst` rest_sub
          updated_args = apply sub infered_arg_types
          updated_funvardecls = apply sub checked_funvardecls
          updated_retty = apply sub infered_retty

          argnames = map fst args
      -- _ <- Debug.trace ("Infered fuction " ++ blue name ++ " as " ++ show (apply sub funtype) ++ " after applying sub ("  ++ show sub ++ ") but we have" ++ show retty ++ " so apply s retty gives " ++ show ( apply sub retty)) pure ()

      return (sub, env, FunDecl meta name updated_retty (zip argnames updated_args) updated_funvardecls (map upgrade body) : checked_program)
-}

instance Types (Decl TypecheckedP) where
      ftv (VarDecl _ _ ty _) = ftv ty
      ftv (FunDecl _ _ retty args funvars _) = ftv (map snd args) <> ftv retty <> ftv funvars -- idk if we should include fun vars here
      apply sub (VarDecl meta name ty  expr) = VarDecl meta name (apply sub ty) expr
      apply sub (FunDecl meta name retty args funvars body) = FunDecl meta name (apply sub retty) (applyToArgs sub args) (apply sub funvars) body

applyToArgs :: Subst -> [(String, Type)] -> [(String, Type)]
applyToArgs s args = let names = map fst args
                         types = map snd args in zip names $ map (apply s) types

instance (Types typeable1, Types typeable2) => Types (typeable1, typeable2) where
      ftv (typeable1, typeable2) = ftv typeable1 <> ftv typeable2 -- This might be wrong because it merges both envs.
      apply sub (typeable1, typeable2) = (apply sub typeable1, apply sub typeable2)

getDeclName :: Decl a -> String
getDeclName (VarDecl _ name _ _) = name
getDeclName (FunDecl _ name _ _ _ _) = name 

-- getDeclType :: Decl TypecheckedP -> Type
-- getDeclType (VarDecl _ _ ty _) = ty
-- getDeclType (FunDecl _ _ retty args _ _) = FunType (map snd args) retty

getReturnType :: Decl TypecheckedP -> Type
getReturnType (VarDecl _ _ _ty _) = error "A var decl has no return type" -- But it can if you want to :) but I don't need that yet
getReturnType (FunDecl _ _ retty _ _ _) = retty

updateFunArgs :: [String] -> Map String Type -> [(String, Type)]
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
insertFunArgsIntoEnv :: Map String Type -> (String, Maybe Type) -> TI (Map String Type)
insertFunArgsIntoEnv _env (name, maybeType) = do
  ty <- maybe newTyVar pure maybeType
  return $ Map.insert name ty _env

-- Check duplicate declerations 
checkDuplicateDecls ::  Program ReturnsCheckedP -> Either String String
checkDuplicateDecls = checkDuplicateDecl' Map.empty Map.empty
  where
        checkDuplicateDecl' _ _ [] = Right "No duplicate declerations"
        checkDuplicateDecl' funmemory varmemory (FunDecl meta name _ _ vardelcs _ : program) =
          checkDuplicateDecls vardelcs >> case Map.lookup name funmemory of
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
defaultFunEnv :: Map String Scheme
defaultFunEnv = Map.fromList [("print", Scheme (Set.singleton "print_inputty") (FunType [TypeVar "print_inputty" False] VoidType))]

defaultVarEnv :: Map String Type
defaultVarEnv = Map.fromList []


-- checkStmp :: Stmt ReturnsCheckedP -> Stmt TypecheckedP
-- checkStmp = 137

-- checkExpr :: Expr ReturnsCheckedP -> Expr TypecheckedP
-- checkExpr = 137


-- Shouldn't are varenv also be Map String Scheme