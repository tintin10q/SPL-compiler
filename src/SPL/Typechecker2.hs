{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant bracket" #-}

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
import SPL.PrettyPrint
import Data.List (intercalate)
import Data.Map (Map)

data Scheme = Scheme (Set.Set String) Type
  deriving (Show, Eq)

type FunEnv = Map String Scheme

type VarEnv = Map String Type

--- Type inference enviroment
data TIenv = TIenv {currentFunEnv :: FunEnv, -- Map String Shceme
                    currentVarEnv :: VarEnv,
                    currentMeta :: SourceSpan,
                    currentFunName :: Maybe String, -- Just for error messages
                    -- currentFunGroup :: Map String Type, -- All functions in the current group that we are checking, this is half way because we need to also only apply at the end
                    currentFunType :: Maybe Type,
                    currentFunArgNames :: Maybe [String],
                    currentTypeVarCounter :: Int,
                    funCallRecord :: Map (String, [Type]) String, -- Function name, list of arguments and the name to replace it with.
                    currentGlobalVarNames :: Set.Set String
                    -- functionsToGenerate :: 
                  }
    deriving Show

emptyTypecheckEnv :: TIenv
emptyTypecheckEnv = TIenv { currentFunEnv = defaultFunEnv, currentVarEnv = Map.empty, currentMeta = undefined,
                            funCallRecord = Map.empty,
                            -- currentFunGroup = Map.empty, -- All functions in the current group that we are checking, this is half way because we need to also only apply at the end
                            currentFunType = Nothing,
                            currentGlobalVarNames = Set.empty,
                            currentFunArgNames = Nothing, currentTypeVarCounter = initTyCounterStart, currentFunName = Nothing}

-- We are now going to solve the instanciate issue

{- A subst is a map from type var names to types-}
type Subst = Map String Type

type FreeTypeVarNames = Set.Set String

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

type TIState = TIenv

type TI a = ExceptT String (State TIState) a

class Types a where
  typevars :: a -> FreeTypeVarNames
  apply :: Subst -> a -> a


instance Types TIenv where
  typevars (TIenv {currentVarEnv = varenv, currentFunType = funtype}) = typevars funtype <> typevars varenv-- Je moet blijven applyen want dan krijg je goede errors 
  apply sub tienv@(TIenv {currentVarEnv = currentvarenv, currentFunType = currentfuntype}) = tienv {currentVarEnv = apply sub currentvarenv, currentFunType = apply sub currentfuntype}

instance (Types a) => Types [a] where
  apply s = map (apply s)
  typevars = foldr (Set.union . typevars) Set.empty

instance Types a => Types (Maybe a) where
  typevars Nothing =  Debug.traceStack (black "hi") error "Trying to get type vars from a nothing!!!!"
  typevars (Just a) = typevars a
  apply sub Nothing =  Debug.trace ("Trying to apply sub "++ show sub ++" on empty maybe") Nothing
  apply sub (Just a) = Just (apply sub a)

{- Functions for the TIenv state monad-}

defaultFunEnv :: Map String Scheme
defaultFunEnv = Map.fromList [("print", Scheme (Set.singleton "print_inputty") (FunType [TypeVar "print_inputty" False] VoidType))]

-- Meta is not defined here but maybe if you don't touch it its ok?

increaseTypeVarCounter :: TI Int
increaseTypeVarCounter = do count <- gets currentTypeVarCounter
                            let new_count = count + 1
                            modify $ \env -> env {currentTypeVarCounter = new_count}
                            return new_count

-- Lookup is when it could fail. (in theory)

-- Function that gets a var from the var env or throw a var not found if it can't
lookupVarType :: String -> TI Type
lookupVarType varname = do
  varenv <- gets currentVarEnv
  case Map.lookup varname varenv of
    Just varT -> return varT
    Nothing -> gets currentMeta >>= \meta -> throwError $ red "Undefined variable " ++ show varname ++ " at " ++ showStart meta ++ "."

lookupFunScheme :: String -> TI Scheme
lookupFunScheme name = do
  funenv <- gets currentFunEnv
  case Map.lookup name funenv of
    Nothing -> gets currentMeta >>= \meta -> throwError $ red "Function '" ++  blue name ++ red "' is undefined. " ++ "At " ++ showStart meta
    Just scheme -> return scheme

lookupCurrentFunName :: TI String
lookupCurrentFunName = do
  name <- gets currentFunName
  case name of
    Nothing -> gets currentMeta >>= \meta -> throwError $ red "No current function name in type inference envrioment. " ++ "This is a compiler error. I don't know the name of the function I am currently checking at " ++ showStart meta
    Just found -> return found

lookupCurrentFunType :: TI Type
lookupCurrentFunType = do
  name <- gets currentFunType
  case name of
    Nothing -> gets currentMeta >>= \meta -> throwError $ red "No current function type in type inference envrioment. " ++ "This is a compiler error. I don't know the type of the function I am currently checking at " ++ showStart meta
    Just found -> return found

lookupArgNames :: TI [String]
lookupArgNames = do
  names <- gets currentFunArgNames
  case names of
    Nothing -> gets currentMeta >>= \meta -> throwError $ red "No current function arg names in type inference envrioment. " ++ "This is a compiler error. I don't know the argument name of the function I am currently checking at " ++ showStart meta
    Just found -> return found

applySubToTIenv :: Subst -> TI ()
-- todo add warning here to show ones that did not do anything? Perf?
applySubToTIenv sub = modify (apply sub)

replaceMeta :: SourceSpan -> TI ()
replaceMeta meta = modify (\env -> env {currentMeta = meta})

replaceCurrentFunName :: String -> TI ()
replaceCurrentFunName name = modify (\env -> env {currentFunName = Just name})

removeCurrentFunName :: TI ()
removeCurrentFunName = modify (\env -> env {currentFunName = Nothing})

replaceCurrentFunArgNames :: [String] -> TI ()
replaceCurrentFunArgNames names = modify (\env -> env {currentFunArgNames = Just names})

removeCurrentFunArgNames :: TI ()
removeCurrentFunArgNames = modify (\env -> env {currentFunArgNames = Nothing})

removeCurrentFunType :: TI ()
removeCurrentFunType = modify (\env -> env {currentFunType = Nothing})


replaceCurrentFunType :: Type -> TI ()
replaceCurrentFunType funty = modify (\env -> env {currentFunType = Just funty})

debugEnv :: String -> TI ()
debugEnv str =do
  Debug.trace (black str) $ pure ()
  env <- get
  Debug.trace ("Env is now:\n" ++ show env) pure ()
  return ()

addScheme :: String -> Scheme -> TI ()
addScheme name scheme = do
  env <- get

  let schemes = currentFunEnv env
  when (name `Map.member` schemes) $ throwError $ "Inserting scheme again for function:"++ blue name
  let schemes' = Map.insert name scheme schemes

  put env {currentFunEnv = schemes'}

newTyVar :: ExceptT String (State TIState) Type
newTyVar = do
  s <- increaseTypeVarCounter
  let name = toTyVar s
  return $ Debug.trace ("Making type var " ++ show name) TypeVar (reverse name) False
  where
    toTyVar c
      | c < 26 = [toEnum (97 + c)]
      | otherwise = let (n, r) = c `divMod` 26 in toEnum (97 + r) : toTyVar (n - 1)

-- Automatically takes care of shadowed variables based on the current global Var names
extendVarEnv :: VarEnv -> TI ()
extendVarEnv new_varenv = 

              modify (\env@TIenv {currentVarEnv = varenv} -> let new = varenv <> new_varenv in Debug.trace ("Extending var env with" ++ show new_varenv ++ "now its "++ show new) env  {currentVarEnv = new })

extendFunEnv :: String -> Scheme -> TI ()
extendFunEnv funname fun_type = modify (\env@TIenv {currentFunEnv = funenv} -> env {currentFunEnv = Map.insert funname fun_type funenv })

resetVarEnv :: VarEnv -> TI ()
resetVarEnv varenv = debugEnv "Before RESET!" >> modify (\env -> env {currentVarEnv = varenv}) >> debugEnv "After RESET!"

-- If the string is the current function return the type of the current function else instanctiate the type scheme and return that. 
lookupFunTypeOf :: String -> TI Type
lookupFunTypeOf name = do
    currentName <- lookupCurrentFunName `catchError` \err -> throwError $ "While looking to get name of current function for a function call ti got error here is that error: \n" ++ err
    if name == currentName then lookupCurrentFunType else lookupFunScheme name >>= instantiate

-- List of global variable names that can be shawed
replaceCurrentGlobalVarNames :: Set.Set String -> TI ()
replaceCurrentGlobalVarNames new_set = modify (\env -> env {currentGlobalVarNames = new_set  } )

-- When a var is added to the vars that is also in global var we remove the var from the global var first. and then we save what we removed into a shadowed map.
-- Then at the end we merge the orignal vars with the shadow map. Lets do code gen now. 
-- Also actually getting the tyep in the decl means like reruning it.

-- But we save that 

{- End of Functions for the TIenv state -}


instance Types Type where
  typevars (TupleType t1 t2) = Set.union (typevars t1) (typevars t2)
  typevars (ListType t) = typevars t
  typevars (FunType args rt) =  typevars args <> typevars rt
  typevars (TypeVar var False) = Set.singleton var
  typevars _ = Set.empty -- No rigid!
  apply subst t@(TypeVar var False) = case Map.lookup var subst of
                                          Nothing -> t; -- Return the same type variable  
                                          Just concrete -> concrete -- Replace type variable with the type of the sub
  apply subst (TupleType t1 t2) = TupleType (apply subst t1) (apply subst t2)
  apply subst (ListType t) = ListType (apply subst t)
  apply subst (FunType args rt) = FunType (apply subst args) (apply subst rt)
  apply _ concreteType = concreteType

-- instance Types Scheme where
  -- typevars (Scheme vars t) = typevars t `Set.difference` vars -- Only return the free variables from t that are in vars, this way you can do rigid without the boolean. Todo, ask, vraag
  -- 
  -- apply s (Scheme vars t) = error "do not apply to schemes" Scheme vars (apply (foldr Map.delete s vars) t)

    -- let scheme_vars_remove = toRemoveFromScheme s
                                -- q = Debug.trace ((show $ Map.keys s) ++ "Not rigid type vars in sub" ++ show typevars_in_s ++ black "apply on scheme" ++ " s: " ++ show s ++ "vars: " ++ show vars ++ "t: " ++ show t ++ " result of fold" ++ show ( (foldr Map.delete s vars) ) )  0
                                -- new_scheme_vars = Set.difference vars scheme_vars_remove
                                  -- in  Scheme new_scheme_vars (apply (foldr Map.delete s new_scheme_vars) t) -- make a new scheme by applying the sub, todo idk why the delete though
  -- We need to delete the type of x (Typevar b) from the free vars in the scheme. Lets do a seperate function for that. The free vars are blocking the further apply!

  -- A type var 

envtolist :: Subst -> [(String, Type)]
envtolist = Map.toList

  -- Delete from the scheme string list the keys from s that are not a type var 
  -- Dit moet alleen als de name in de fun env matched met Var, dan moet je de type scheme wel narroewen!!! En dan denk ik permanent. 
toRemoveFromScheme :: Subst -> Set.Set String
toRemoveFromScheme = toRemoveFromScheme' . Map.toList
  where
        toRemoveFromScheme' :: [(String, Type)] -> Set.Set String
        toRemoveFromScheme' [] = Set.empty
        toRemoveFromScheme' ((tyvarname , ty):rest) = if Set.size (typevars ty) == 0 then tyvarname `Set.insert` toRemoveFromScheme' rest--n : getNonRigidTypevars rest
                                                                                else toRemoveFromScheme' rest

        -- toRemoveFromScheme' :: [(String, Type)] -> Set.Set String
        -- toRemoveFromScheme' [] = Set.empty
        -- toRemoveFromScheme' ((_, TypeVar n false):rest) = toRemoveFromScheme' rest
        -- toRemoveFromScheme' ((tyvarname , ty):rest) = tyvarname `Set.insert` toRemoveFromScheme' rest--n : getNonRigidTypevars rest

-- toRemoveFromScheme (_:rest) = getNonRigidTypevars rest


-- The instantiation function replaces all bound type variables in
--  a type scheme with fresh type variables.
-- So with this you can make an instance of a type scheme
instantiate :: Scheme -> TI Type
instantiate (Scheme vars ty) = do
  nvars <- mapM (const newTyVar) (Set.toList vars) -- newTyVar for each var in the scheme
  let s = Map.fromList (zip (Set.toList vars) nvars) -- todo make this nice r
   in return $ apply s ty

varBind :: SourceSpan -> String -> Type -> TI Subst
varBind meta u t
  | u `Set.member` typevars t =
      throwError $
        red "Occurs check:" ++ " cannot construct the infinite type " ++ u ++ " ~ " ++ show t ++ " at " ++ showStart meta
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
unify  (TypeVar u1 False) t2@(TypeVar _ False) = return $ Map.fromList [(u1, t2)]  -- U (α, α) = id
unify  (TypeVar u False) t = gets currentMeta >>= \meta -> varBind meta u t -- bind the type var!
unify t (TypeVar u False) = gets currentMeta >>= \meta -> varBind meta u t
unify ty1@(TypeVar name1 True) ty2@(TypeVar name2 True) = if name1 == name2 then return nullSubst else unifyError ty1 ty2
unify ty1@(TypeVar _ True) ty2 = unifyError ty1 ty2
unify ty1 ty2@(TypeVar _ True) = unifyError ty1 ty2
unify (ListType t1) (ListType t2) = unify t1 t2
unify (TupleType t1 t2) (TupleType t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  let s = s1 `composeSubst` s2
  applySubToTIenv s
  return s
unify IntType IntType = return nullSubst
unify BoolType BoolType = return nullSubst
unify CharType CharType = return nullSubst
unify VoidType VoidType = return nullSubst
-- todo probably add case for Void Void 
unify t1 t2 = unifyError t1 t2

class Typecheck a where
  {-# MINIMAL tc | ti #-}
  tc :: a -> Type -> TI Subst
  tc a t = do
    (s1, inferredT) <- ti a
    s2 <- unify t inferredT
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return s
  ti :: a -> TI (Subst, Type)
  ti a = do
    t <- newTyVar
    s <- tc a t
    applySubToTIenv s
    return (s, apply s t)

instance Typecheck (Literal TypecheckedP) where
  ti (IntLit _) = return (nullSubst, IntType)
  ti TrueLit = return (nullSubst, BoolType)
  ti FalseLit = return (nullSubst, BoolType)
  ti (CharLit _) = return (nullSubst, CharType)
  ti EmptyListLit = do
    var <- newTyVar
    return (nullSubst, ListType var) --  applying s here beaks conconscons exapmle so don't
  ti (TupleLit (e1, e2)) = do
    (s1, t1) <- ti e1
    (s2, t2) <- ti e2
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, TupleType t1 t2)

{- Check if the type of 2 expr can be CheckType return the optained substitution if they can -}
tcBinOp :: Type -> Expr TypecheckedP -> Expr TypecheckedP -> TI (Subst)
tcBinOp checkType e1 e2 = do
  s1 <- tc e1 checkType
  s2 <- tc e2 checkType
  let s = s1 `composeSubst` s2
  applySubToTIenv s
  return s 

-- todo Dependent types for booleans...? 

-- First tries to infer two expressions as 2 ints and then as 2 chars if both fail an error is thrown using throwError
tcBinOpCharOrInt :: BinOp -> Expr TypecheckedP -> Expr TypecheckedP -> TI (Subst)
tcBinOpCharOrInt op e1 e2 = tcBinOp IntType e1 e2 `catchError` 
                                \_ -> tcBinOp CharType e1 e2  `catchError`
                                  \_ -> do
                                    (_, t1) <- ti e1 -- Get the type for the error
                                    (_, t2) <- ti e2
                                    meta <- gets currentMeta
                                    errorTail <- unifyError t1 t2
                                    throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to " ++ pretty op ++" should be either Int or Char but you gave " ++ show t1 ++ show op ++ show t2 ++ errorTail


tiBinaryIntOp :: Expr TypecheckedP -> Expr TypecheckedP -> Type -> TI (Subst, Type)
tiBinaryIntOp e1 e2 ty = do 
  s1 <- tcBinOp IntType e1 e2
  s2 <- unify ty IntType
  let s = s1 `composeSubst` s2
  applySubToTIenv s -- We could put the whole decl in there and keep applying it!
  return (s, IntType) -- No need to apply s here, it will be done at the end

-- Checks if two expressions can be bool and if the type of the node can be bool
tiBinaryBoolOp :: Expr TypecheckedP -> Expr TypecheckedP -> Type -> TI (Subst, Type)
tiBinaryBoolOp e1 e2 ty = do 
  s1 <- tc e1 BoolType
  s2 <- tc e2 BoolType
  s3 <- unify ty BoolType -- check type of the node
  let s = s1 `composeSubst` s2 `composeSubst` s3
  applySubToTIenv s -- We could put the whole decl in there and keep applying it!
  return (s, BoolType) -- No need to apply s here, it will be done at the end


{- An operation that needs equal types but you don't know which ones. This we might also have to see as a function application to get good polymorpic types, the result is bool anyways!
We are checking if the two types of the expressions are equal in a sense -}
tiEqualityOp :: Expr TypecheckedP -> Expr TypecheckedP -> Type -> TI (Subst, Type)
tiEqualityOp e1 e2 ty = do 
  (s1, ty1) <- ti e1 
  (s2, ty2)  <- ti e2 
  s3 <- unify ty1 ty2 -- todo this could be a nicer error message like you can only compare things of the same type? 
  s4 <- unify ty BoolType
  let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
  applySubToTIenv s -- We could put the whole decl in there and keep applying it!
  return (s, BoolType) 

tiCharOrIntToBoolOp :: Type -> Expr TypecheckedP -> BinOp -> Expr TypecheckedP -> TI (Subst, Type)
tiCharOrIntToBoolOp ty e1 op e2 = do 
      s1 <- tcBinOpCharOrInt op e1 e2
      s2 <- unify ty BoolType
      let s = s1 `composeSubst` s2
      applySubToTIenv s
      return (s, BoolType)

instance Typecheck (Expr TypecheckedP) where
  -- ti (LiteralExpr (ty, meta) lit) = replaceMeta meta >> ti lit 
  ti (LiteralExpr (ty, meta) lit) = do 
    replaceMeta meta 
    (s1, ty') <- ti lit
    s2 <- unify ty ty' -- Fix the type of the node
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, ty')

  -- Cons
  ti (BinOpExpr (ty, meta) Cons e1 e2) = do
    replaceMeta meta
    (s1, t1) <- ti e1
    (s2, listty) <- ti e2
    -- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++  " s2: " ++ show s2 ) pure ()
    s3 <- case listty of -- If t2 is a list type we have to unify with the type inside the list
            ListType u1 -> unify t1 u1  `catchError` \err -> throwError $ "You tried to cons " ++ show t1 ++ " with " ++ show listty ++ ", but this is not legal.\n" ++ err
            _ -> throwError $ red "You tried to " ++ bold "cons" ++ " " ++ show listty ++ red " with "++ show t1 ++" but that does work. The right of a cons should always be a list type."   
    let inferredType = ListType t1
    s4 <- unify ty inferredType -- Check the type of this node and save the result
    let s = s1 `composeSubst` s2 `composeSubst` s3 `composeSubst` s4
    -- Debug.trace ("Doing Cons " ++ show t1 ++ " : " ++ show t2 ++ show s4 ++ " s4!" ++ "s: " ++ show s ) pure ()
    applySubToTIenv s
    return (s, apply s inferredType) -- To be sure apply this already here to narrow, todo but not really needed

  ti (BinOpExpr (ty, meta) Mul e1 e2) = replaceMeta meta >> tiBinaryIntOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Mod e1 e2) = replaceMeta meta >> tiBinaryIntOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Add e1 e2) = replaceMeta meta >> tiBinaryIntOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Div e1 e2) = replaceMeta meta >> tiBinaryIntOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Sub e1 e2) = replaceMeta meta >> tiBinaryIntOp e1 e2 ty

  ti (BinOpExpr (ty, meta) Neq e1 e2) = replaceMeta meta >> tiEqualityOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Eq e1 e2) = replaceMeta meta >> tiEqualityOp e1 e2 ty

  ti (BinOpExpr (ty, meta) And e1 e2) = replaceMeta meta >> tiBinaryBoolOp e1 e2 ty
  ti (BinOpExpr (ty, meta) Or e1 e2) = replaceMeta meta >> tiBinaryBoolOp e1 e2 ty

  -- Check Lte, Gt, Lt and Lt 
  -- In these last ones we could make the op a var and make them the same but more explicit is better
  ti (BinOpExpr (ty, meta) Gte e1 e2) = replaceMeta meta >> tiCharOrIntToBoolOp ty e1 Gte e2 
  ti (BinOpExpr (ty, meta) Lte e1 e2) = replaceMeta meta >> tiCharOrIntToBoolOp ty e1 Lte e2
  ti (BinOpExpr (ty, meta) Gt e1 e2) = replaceMeta meta >> tiCharOrIntToBoolOp ty e1 Lt e2
  ti (BinOpExpr (ty, meta) Lt e1 e2) = replaceMeta meta >> tiCharOrIntToBoolOp ty e1 Lt e2

  ti (UnaryOpExpr (ty, meta) Negate e) = do
    replaceMeta meta
    s1 <- tc e BoolType
    s2 <- unify ty BoolType
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, BoolType)
  ti (UnaryOpExpr (ty,meta) Min e) = do
    replaceMeta meta
    s1 <- tc e IntType 
    s2 <- unify ty IntType 
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return (s, IntType)
  ti (UnaryOpExpr (nodeTy, meta) (FieldAccess field) expr) = do 
      replaceMeta meta
      (s1, inferredType) <- checkFieldAccess expr field -- This gets the type of the expr and check if its a list type or tuple type if not then its an error
      s2 <- unify inferredType nodeTy -- fix the node type after figering out the field
      let s = s1 `composeSubst` s2
      applySubToTIenv s
      return (s, inferredType)
  ti (VariableExpr (nodeTy, meta) (Identifier var (Just field))) = do
      replaceMeta meta
      -- Send a var expr
      let expr = VariableExpr (nodeTy, meta) (Identifier var Nothing)
      (s1, inferredType) <- checkFieldAccess expr field
      s2 <- unify inferredType nodeTy -- fix the node type after figering out the field
      let s = s1 `composeSubst` s2
      applySubToTIenv s
      return (s, inferredType)
  ti (VariableExpr (ty, meta) (Identifier var Nothing)) = do 
    replaceMeta meta 
    sigma <- lookupVarType var 
    s <- unify ty sigma
    applySubToTIenv s
    return (s, sigma)
  ti (FunctionCallExpr (ty, meta) "print" args) = do 
    replaceMeta meta
     -- Print is special just return Void, later when we have the types of all expr we rewrite it to the right one! 
       -- To do that we do need to know the type of the arga
    arg <- case args of 
      [] ->  throwError $ "You called the print function with no arguments at " ++ showStart meta ++ ". The isEmpty function takes exactly one argument. The argument should be either a list or a tuple."
      (_:_:_) -> throwError $ "You called the isEmpty function with too many arguments at " ++ showStart meta ++ ". The isEmpty function takes exactly one argument. The argument should be either a list or a tuple."
      [arg] -> pure arg
    
    (s1, _) <- ti arg -- we need the sub for the type var in the argument
    s2 <- unify ty VoidType 
    let s = s1 `composeSubst` s2
    applySubToTIenv s
    return  (s, VoidType) 

  ti (FunctionCallExpr (ty, meta) "isEmpty" args) = do
    replaceMeta meta 
    arg <- case args of 
      [] ->  throwError $ "You called the isEmpty function with no arguments at"  ++ showStart meta ++ ". The isEmpty function takes exactly one argument. The argument should be either a list or a tuple."
      (_:_:_) -> throwError $ "You called the isEmpty function with too many arguments at " ++ showStart meta ++ ". The isEmpty function takes exactly one argument. The argument should be either a list or a tuple."
      [arg] -> pure arg

    -- todo not so nice that we need to have a new type var for every print check, we could also infer first check if its a list and only if not introduce a type var
    tyvar <- newTyVar
    s1 <- tc arg (ListType tyvar) `catchError` \e -> throwError ("You can only call isEmpty on list types.'\n" ++ e) -- we need the sub for the type var in the argument
    s2 <- unify ty BoolType
    let s = s1  `composeSubst` s2
    applySubToTIenv s
    return (s, BoolType) -- isEmpty is special just return Void, but we should actually infer if its a container type 
  ti (FunctionCallExpr (ty, meta) funcName args) = do
    replaceMeta meta
    funtype <- lookupFunTypeOf funcName
    (funargs, retty) <- case funtype of
          FunType funargs retty -> Debug.trace (black "got funtype of " ++ funcName ++ " as " ++ show funtype ) pure (funargs, retty)
          ty -> error $ ("Did not get a funtype after instantiate scheme, how does that happen!!" ++ show ty ++ " ") ++ showStart meta

    let arg_count = length funargs
        n_given_args = length args
    when (n_given_args > arg_count) (throwError $ red "You called the '"++ blue funcName ++ red "' function at " ++ showStart meta ++ red " with too many arguments " ++ "("++ green ( show n_given_args )++ " > " ++ green (show arg_count) ++ ")" ++ "\nThe '"++ blue funcName ++ "' function takes " ++ green ( show arg_count) ++ " argument" ++ (if arg_count == 1 then "" else "s") ++ " but you gave " ++ green ( show n_given_args) ++ " argument" ++ (if n_given_args == 1 then "" else "s") ++".")
    when (n_given_args < arg_count) (throwMissingArgs $ arg_count - n_given_args)


    args_subs <- zipWithM tc args funargs
    let args_sub = foldr composeSubst nullSubst args_subs -- combine arg subs 
        resultingType = apply args_sub retty
    rettysub <- unify ty resultingType -- todo test the error messages this gives. 
    let sub = args_sub `composeSubst` rettysub
    applySubToTIenv sub
    return (sub, resultingType) -- We apply it so that the receiver of this type gets the latest version 
      where
        -- This is used for the error message
        throwMissingArgs :: Int -> TI ()
        throwMissingArgs missing = do
          funcname <- lookupCurrentFunName
          names <- if funcname == funcName then lookupArgNames >>= \argnames -> pure $ "\nThe missing arguments are called " ++ intercalate " and " (map blue (drop missing argnames)) ++ "." else pure ""
          throwError $ red "Function call at " ++ showStart meta ++ red " is missing " ++ green (show missing) ++ red " argument" ++ if missing == 1 then "" else "s"++ "." ++ names

-- Check if a field fits with the type of the expression is on 
-- Then gives back the correct type from the expression
checkFieldAccess :: Expr TypecheckedP -> Field -> TI (Subst, Type)
checkFieldAccess expr field = do
  (sub, ty) <- ti expr
  case field of 
        FirstField -> case ty of 
          TupleType t1 _ -> return (sub, t1)
          (ListType _) -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty field ++  red" field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty field ++ " field on tuple types." ++ "\nMaybe you meant" ++ pretty HeadField ++  "?"
          tyvar@(TypeVar _ False) -> newTyVar >>= \secondArgTyvar -> unify (TupleType ty secondArgTyvar) tyvar >>= \s -> applySubToTIenv s >> pure (s, ty)
          ty2 -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty ty2 ++ " red field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty ty2 ++ " field on tuple types."

        SecondField -> case ty of 
          TupleType _ t2 -> return (sub, t2)
          (ListType _) -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty field ++  red" field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty field ++ " field on tuple types." ++ "\nMaybe you meant" ++ pretty  TailField ++  "?"
          tyvar@(TypeVar _ False) -> newTyVar >>= \secondArgTyvar -> unify (TupleType ty secondArgTyvar) tyvar >>= \s -> applySubToTIenv s >> pure (s, ty)
          ty2 -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty ty2 ++ " red field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty ty2 ++ " field on tuple types."

        HeadField -> case ty of 
          ListType t -> return (sub, t)
          TupleType {} -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty field ++  red" field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty field ++ " field on list types." ++ "\nMaybe you meant" ++ pretty  FirstField ++  "?"
          tyvar@(TypeVar _ False) -> unify (ListType ty) tyvar >>= \s -> applySubToTIenv s >> pure (s, ty)
          ty2 -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty ty2 ++ " red field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty ty2 ++ " field on list types."

        TailField -> case ty of 
          ListType t -> return (sub, ListType t)
          TupleType {} -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty field ++ red" field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty field ++ " field on list types." ++ "\nMaybe you meant" ++ pretty  SecondField ++  "?"
          tyvar@(TypeVar _ False) -> unify (ListType ty) tyvar >>= \s -> applySubToTIenv s >> pure (s, ty)
          ty2 -> gets currentMeta >>= \meta -> throwError $ red "You accessed the " ++ pretty ty2 ++ " red field on a " ++ pretty ty ++ red " at " ++ showStart meta ++ red ". " ++ "But that is invalid you can only access the " ++ pretty ty2 ++ " field on list types."
  

instance Typecheck (Stmt TypecheckedP) where
  ti (ReturnStmt meta (Just expr)) = do
    replaceMeta meta
    funtype <- lookupCurrentFunType  `catchError` \err -> throwError $ err ++ "\nNo function name, that means the return at "++ showStart meta ++ red " is outside a function? Don't do that."

    (_, returntype) <- case funtype of -- don't generalize this one I think, otherwise you still don't really know if you have retty and arg types
                                  FunType argtypes returntype -> pure (argtypes,returntype)
                                  t -> throwError $ "Function env returned non function type " ++ show t

    s1 <- tc expr returntype --`catchError` \err -> lookupCurrentFunName >>= \funname -> throwError $ red "Returned expression at " ++ showEnd meta ++ red " in function " ++ blue funname ++ red "' has an invalid type. " ++ show returntype ++ ".\n" ++ err
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
  ti (AssignStmt meta (Identifier var (Just field)) expr) = do
      replaceMeta meta
      varT <- lookupVarType var
      (s1, exprType) <- checkFieldAccess expr field
      s2 <- unify varT exprType
      let s  = s1 `composeSubst` s2
      applySubToTIenv s
      return (s, VoidType)
  ti (AssignStmt meta (Identifier var Nothing) expr) = do
    replaceMeta meta
    (s1, t) <- ti expr
    varT <- lookupVarType var
    -- Debug.trace (black "asign inffert assign expr expr"++ pretty expr ++" as " ++ show t ++ " type of var was" ++ show varT) pure ()
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
instance Typecheck [Stmt TypecheckedP] where

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
instance Typecheck (Decl TypecheckedP) where
  ti (VarDecl meta name ty expr) = do
    Debug.trace ("Inferencing decl " ++ blue name ) pure ()
    replaceMeta meta
    sub <- tc expr ty
    -- debugEnv "Exstending the type env"
    extendVarEnv (Map.fromList [(name, ty)])
    -- debugEnv "After Exstending the type env"
    applySubToTIenv sub >> pure (sub, ty)
  ti (FunDecl meta name initial_retty args fundelcs body) = do -- The returned sub contains the information of the args, decls and body, the caller does not need to add anything to the env
      -- Debug.trace (blue ("Inferencing function '" ++ name ++ "': ")) pure () -- this was as tight as I could make it

      global_vars <- gets currentVarEnv -- Save the current vars, however. We do need to propagate the changes from the body to this

      replaceMeta meta -- Set error generation information
      replaceCurrentFunName name

      let argnames = map fst args  -- Extend var env with the arguments
      replaceCurrentFunArgNames argnames -- Replace current funarg names for error messages

      -- Make the initial function type 
      let initial_fun_type = FunType (map snd args) initial_retty
      replaceCurrentFunType initial_fun_type

      let argvar = Map.fromList args
      extendVarEnv argvar

      fundelc_inference <- mapM ti fundelcs -- These extends the var env with the arguments by itself

      let infered_fundelc_subs = map fst fundelc_inference -- Create the substitution for the decls
          infered_fundelc_sub = foldr composeSubst nullSubst infered_fundelc_subs -- I don't think foldMap works here

      applySubToTIenv infered_fundelc_sub -- Not needed because ti does it already doesn't hurt.

      infered_body_sub <- tc body initial_retty

      let function_sub = infered_fundelc_sub `composeSubst` infered_body_sub

      current_function_type <- lookupCurrentFunType

      let type_scheme = Scheme (typevars current_function_type) current_function_type -- Save the scheme
      extendFunEnv name type_scheme
      -- debugEnv "State right before reste"

      removeCurrentFunName -- reset scope combined action??
      removeCurrentFunArgNames
      resetVarEnv $ apply function_sub global_vars -- Also propagate the found things to the global vars , but to get that in the decls we have to apply again at the end of check program

      return (function_sub, apply function_sub current_function_type)


getRelevantVars :: Type -> TI String
getRelevantVars ty = do
   varEnv <- gets currentVarEnv
   let relevant = Map.keys $ Map.filter (ty ==) varEnv in
      if null relevant  then pure "" else pure $ "\nPotential variables of type " ++ show ty ++ " in scope include " ++ printWithCommas (typeListToStrings relevant)
  where typeListToStrings = map (\r -> '\'' : blue r ++ "'")

unifyErrorHead :: TI String 
unifyErrorHead = do 
  functionName <- gets currentFunName
  case functionName of 
    Just name -> pure $ "Inside function "++blue name ++":\n"
    Nothing -> pure ""


unifyError :: Type -> Type -> ExceptT String (State TIState) b
unifyError ty1@(TypeVar name1 True) ty2@(TypeVar name2 True) = do
  errorHead <- unifyErrorHead
  errorTail <- unifyErrorTail ty1 ty2
  throwError $  errorHead ++ "Cannot unify " ++ show ty1 ++ " with " ++ show ty2 ++ " as '" ++ blue name1 ++ "' and '" ++ blue name2 ++ "' are two " ++ bold "different" ++ " rigid type variable because they have a different name.\nA rigid type variable means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty2@(TypeVar u True) ty1 = do
  errorHead <- unifyErrorHead
  errorTail <- unifyErrorTail ty1 ty2
  throwError $ errorHead ++ "Cannot unify " ++ show ty1 ++ " with " ++ show ty2 ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty1 ty2@(TypeVar u True) = do
  errorHead <- unifyErrorHead
  errorTail <- unifyErrorTail ty1 ty2
  debugEnv "right before crash"
  throwError $ errorHead ++ "Types do not unify:\n" ++ (show ty1) ++ " vs. " ++ show ty2 ++ "Cannot unify " ++ show ty2 ++ " with " ++ show ty1 ++ ", as '" ++ blue u ++ "' is a rigid type variable.\nA rigid type variable (in this case " ++ u ++ ") means that the caller can choose the type. Because of this we don't know which operations are possible and thus cannot unify further.\nThis happened" ++ errorTail
unifyError ty1 ty2 = unifyErrorHead >>= \errorHead -> unifyErrorTail ty1 ty2 >>= \errorTail -> throwError $ errorHead ++ "Types do not unify:\n" ++ show ty1 ++ " vs. " ++ show ty2 ++ errorTail

unifyErrorTail :: Type -> Type -> TI String
unifyErrorTail ty1 _ty2 = do
  relevant1 <- getRelevantVars ty1
  -- relevant2 <- getRelevantVars ty2
  meta <- gets currentMeta
  return $ " at " ++ showStart meta ++ " until line " ++ (case endPos meta of SourcePos _ b a -> show (unPos b) ++ " column " ++ show (unPos a)) ++ relevant1 -- ++ relevant2 -- Only first one is really 
            ++ ".\n\nlearn more about unification errors here: https://en.wikipedia.org/wiki/unification_(computer_science) or here https://cloogle.org/#unify%20error"


instance Types VarEnv where
  typevars env = typevars $ Map.elems env
  apply sub = Map.map (apply sub)

-- If its a partial thing it should be between brackets 


-- Ok so with apply with apply the substitutions to something!
-- todo We could also do this with a decl???

-- Checkprogram only adds the vardels and fundecls to the returend funenv and varenv, not the funvardecls ofcourse.

-- checkProgram :: Program ReturnsCheckedP -> Either String (Program TypecheckedP)
checkProgram :: Program 'ReturnsCheckedP -> (Either String (Program TypecheckedP), TIState)
checkProgram program = runTI (instantiateProgram program >>= checkDecls)

checkDecls :: Program TypecheckedP -> TI (Program TypecheckedP)
checkDecls [] = return []
checkDecls (decl:future) = do
  (sub, ty) <- ti decl
  solved <- checkDecls future
  return $ apply sub decl : solved


instance Types (Decl TypecheckedP) where
  typevars (VarDecl _ _ ty _) = typevars ty
  typevars (FunDecl _ _ retty args funvars body) = typevars (map snd args) <> typevars retty <> typevars funvars <> typevars body
  apply sub (VarDecl meta name ty expr) = VarDecl meta name (apply sub ty) (apply sub expr)
  apply sub (FunDecl meta name retty args funvars body) = FunDecl meta name (apply sub retty) (applyToArgs sub args) (apply sub funvars) (apply sub body)

instance Types (Stmt TypecheckedP) where
  apply s (AssignStmt meta var e) = (AssignStmt meta var (apply s e))
  apply s (ReturnStmt meta maybe_value)= (ReturnStmt meta (apply s maybe_value))
  apply s (IfStmt meta cond body alternative) = (IfStmt meta (apply s cond) (map (apply s) body) (apply s alternative))
  apply s (WhileStmt meta e body) = (WhileStmt meta e (apply s body))
  apply s (ExprStmt meta e) = (ExprStmt meta (apply s e))
  apply s (BlockStmt list) = (BlockStmt (apply s list))
  typevars (AssignStmt _ _ e) = typevars e
  typevars (ReturnStmt _ e) = typevars e
  typevars (IfStmt _ cond body alternative) = typevars cond <> typevars body <> typevars alternative
  typevars (WhileStmt _ cond body) = typevars cond <> typevars body
  typevars (ExprStmt _ e) = typevars e
  typevars (BlockStmt list) = typevars list

instance Types (Expr TypecheckedP) where
  typevars (BinOpExpr (ty, _) _ e1 e2) = typevars ty <> typevars e1 <> typevars e2
  typevars (UnaryOpExpr (ty, _) _ e) = typevars ty <>  typevars e
  typevars (FunctionCallExpr (ty, _) _ args) = typevars ty <> foldMap typevars args
  typevars (VariableExpr (ty, _) _) = typevars ty
  typevars (LiteralExpr (ty, _) lit) = typevars ty <> typevars lit
  apply s (BinOpExpr (ty, meta) op e1 e2) = (BinOpExpr (apply s ty, meta) op (apply s e1) (apply s e2))
  apply s (UnaryOpExpr (ty, meta) op e) = (UnaryOpExpr (apply s ty, meta) op e)
  apply s (FunctionCallExpr (ty, meta) name args) = (FunctionCallExpr (apply s ty, meta) name (map (apply s) args))
  apply s (VariableExpr (ty, meta) var) = (VariableExpr (apply s ty, meta) var)
  apply s (LiteralExpr (ty, meta) lit) = (LiteralExpr (apply s ty, meta) (apply s lit))


instance Types (Literal TypecheckedP) where
  typevars (TupleLit (e1, e2)) = typevars e1 <> typevars e2
  typevars _ = Set.empty
  apply s (TupleLit (e1, e2)) = (TupleLit (apply s e1, apply s e2))
  apply _ lit = lit

applyToArgs :: Subst -> [(String, Type)] -> [(String, Type)]
applyToArgs s args = let names = map fst args
                         types = map snd args in zip names $ map (apply s) types

instance (Types typeable1, Types typeable2) => Types (typeable1, typeable2) where
      typevars (typeable1, typeable2) = typevars typeable1 <> typevars typeable2 -- This might be wrong because it merges both envs.
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

varDeclTypeNotFoundError :: Show a1 => String -> a1 -> a2
varDeclTypeNotFoundError name env = error $ "Type of " ++ blue name ++ " not found in type envrioment. This should not happen. It probably was never added to the variable enviroment " ++ show env

mergeTypesGlobalvars :: Program ReturnsCheckedP -> VarEnv -> Program ReturnsCheckedP
mergeTypesGlobalvars [] _ = []
mergeTypesGlobalvars (f@FunDecl {} : rest) env = f : mergeTypesGlobalvars rest env
mergeTypesGlobalvars ((VarDecl meta name _ expr) : rest) env =
  let justty = Map.lookup name env
  -- ty = fromMaybe (varDeclTypeNotFoundError name env) justty
   in VarDecl meta name justty expr : mergeTypesGlobalvars rest env

-- todo Later this one shall have the buildins
--  Phase that we check that none of the user functions are named the default names!

-- checkStmp :: Stmt ReturnsCheckedP -> Stmt TypecheckedP
-- checkStmp a = 137

-- checkExpr :: Expr ReturnsCheckedP -> Expr TypecheckedP
-- checkExpr LiteralExpr _ = 137


-- Shouldn't are varenv also be Map String Scheme
runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) emptyTypecheckEnv

evalTI :: TI a -> Either String a -- Like run but we throw away the state
evalTI t = fst $ runTI t

initTyCounterStart :: Int
initTyCounterStart = 1


instantiateExpr :: Expr ReturnsCheckedP -> TI (Expr TypecheckedP)
instantiateExpr (BinOpExpr meta op e1 e2) = do 
  e1' <- instantiateExpr e1
  e2' <- instantiateExpr e2
  newTy <- newTyVar
  return $ BinOpExpr (newTy, meta) op e1' e2' 
instantiateExpr (UnaryOpExpr meta op e) = do 
  e' <- instantiateExpr e
  newTy <- newTyVar
  return $ UnaryOpExpr (newTy, meta) op e'
instantiateExpr (FunctionCallExpr meta name exprs) = do 
  exprs' <- mapM instantiateExpr exprs 
  newTy <- newTyVar
  return $ FunctionCallExpr (newTy, meta) name exprs'
instantiateExpr (VariableExpr meta var) = do 
  newTy <- newTyVar
  return $ VariableExpr (newTy, meta) var
instantiateExpr (LiteralExpr meta lit) = do 
  newTy <- newTyVar
  lit' <- instantiateLiteral lit
  return $ LiteralExpr (newTy, meta) lit'

instantiateLiteral :: Literal ReturnsCheckedP -> TI (Literal TypecheckedP)
instantiateLiteral (TupleLit (e1, e2)) = do 
  e1' <- instantiateExpr e1
  e2' <- instantiateExpr e2
  return $ TupleLit (e1', e2')
instantiateLiteral TrueLit = return TrueLit
instantiateLiteral FalseLit = return FalseLit
instantiateLiteral (IntLit i) = return $ IntLit i
instantiateLiteral (CharLit c) = return $ CharLit c
instantiateLiteral (EmptyListLit) = return EmptyListLit

instantiateStmt :: Stmt ReturnsCheckedP -> TI (Stmt TypecheckedP)
instantiateStmt (AssignStmt meta var e) = instantiateExpr e >>= \e' -> return $ AssignStmt meta var e'
instantiateStmt (ReturnStmt meta (Just e)) = instantiateExpr e >>= \e' -> return $ ReturnStmt meta (Just e')
instantiateStmt (ReturnStmt meta Nothing) = return $ ReturnStmt meta Nothing
instantiateStmt (IfStmt meta e body Nothing) = do 
  e' <- instantiateExpr e
  body' <- mapM instantiateStmt body
  return $ IfStmt (meta :: IfStmt TypecheckedP) e' body' Nothing
instantiateStmt (IfStmt meta e body (Just alternative)) = do
  e' <- instantiateExpr e
  body' <- mapM instantiateStmt body
  alternative' <- mapM instantiateStmt alternative
  return $ IfStmt (meta :: IfStmt TypecheckedP) e' body' (Just alternative')
instantiateStmt (WhileStmt meta e body) = do 
  e' <- instantiateExpr e
  body' <- mapM instantiateStmt body
  return $ WhileStmt (meta :: WhileStmt TypecheckedP) e' body' 
instantiateStmt (ExprStmt meta e) = instantiateExpr e >>= \e' -> return $ ExprStmt (meta :: ExprStmt TypecheckedP) e'
instantiateStmt (BlockStmt list) = mapM instantiateStmt list >>= \list' -> return $ BlockStmt list'


instantiateProgram :: Program ReturnsCheckedP -> TI (Program TypecheckedP)
instantiateProgram [] = pure []
instantiateProgram (VarDecl meta name maybeTy expr:other) = do
  theType <- maybe newTyVar pure maybeTy
  expr' <- instantiateExpr expr
  later <- instantiateProgram other
  return $ VarDecl meta name theType expr' : later
instantiateProgram (FunDecl meta name maybeRetty args funvardecl body :other) = do
  instanciated_arg_types <- mapM (maybe newTyVar pure . snd) args -- Get the args 
  let arg_names = map fst args
      instanciated_args = zip  arg_names instanciated_arg_types

  instanciated_retty <- maybe newTyVar pure maybeRetty
  instanciated_fundecls <- instantiateProgram funvardecl

  
  instanciated_body <- mapM instantiateStmt body

  later <- instantiateProgram other
  return $ FunDecl meta name instanciated_retty instanciated_args instanciated_fundecls instanciated_body : later


