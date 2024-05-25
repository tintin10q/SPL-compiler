{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.Typechecker2 where


import SPL.AST
import SPL.Colors (blue)
import SPL.Parser.SourceSpan (SourceSpan, endPos, showEnd, showStart)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Megaparsec (SourcePos (SourcePos), unPos)
import SPL.PrettyPrint (prettyPrintMap)

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

class Typecheck a where
  {-# MINIMAL tc | ti #-}
  tc :: (FunEnv, VarEnv) -> SourceSpan -> a -> Type -> TI Subst
  tc env meta a t = do
    (s1, inferredT) <- ti env meta a
    s2 <- unify meta t inferredT
    return $ s1 `composeSubst` s2
  ti :: (FunEnv, VarEnv) -> SourceSpan -> a -> TI (Subst, Type)
  ti env meta a = do
    t <- newTyVar
    s <- tc env meta a t
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

unify :: SourceSpan -> Type -> Type -> TI Subst
unify meta (FunType at decls r) (FunType at' decls' r') = do
  -- We need to unify all the arguments from l with l' pair by pair
  let unify' = unify meta
  s1 <- zipWithM unify' at at' -- same as: mapM (uncurry unify) (zip at at') -- No I would have never have known that witout the hint thing from vscode

  let s1' = foldr composeSubst nullSubst s1 
  s2 <- zipWithM unify' decls decls' 

  let s2' = foldr composeSubst nullSubst s2 -- fold all the argument subs together
  s3 <- unify meta (apply s1' r) (apply s1' r')
  return $ s1' `composeSubst` s2' `composeSubst` s3
unify meta (TypeVar u False) t = varBind meta u t
unify meta t (TypeVar u False) = varBind meta u t
unify meta (TypeVar u True) t =
  throwError $
    "Cannot unify " ++ u ++ " with " ++ show t ++ ", as " ++ u ++ " is a rigid type variable." ++ formatUnifyError meta
unify meta t (TypeVar u True) =
  throwError $
    "Cannot unify " ++ u ++ " with " ++ show t ++ ", as " ++ u ++ " is a rigid type variable." ++ formatUnifyError meta
unify meta (ListType t1) (ListType t2) = unify meta t1 t2
unify meta (TupleType t1 t2) (TupleType t1' t2') = do
  s1 <- unify meta t1 t1'
  s2 <- unify meta (apply s1 t2) (apply s1 t2')
  return $ s1 `composeSubst` s2
unify _ IntType IntType = return nullSubst
unify _ BoolType BoolType = return nullSubst
unify _ CharType CharType = return nullSubst
unify meta t1 t2 = throwError $ "Types do not unify:\n" ++ show t1 ++ " vs. " ++ show t2 ++ formatUnifyError meta

instance Typecheck (Literal ReturnsCheckedP) where
  ti _ _ (IntLit _) = return (nullSubst, IntType)
  ti _ _ TrueLit = return (nullSubst, BoolType)
  ti _ _ FalseLit = return (nullSubst, BoolType)
  ti _ _ (CharLit _) = return (nullSubst, CharType)
  ti _ _ EmptyListLit = do
    var <- newTyVar
    return (nullSubst, ListType var) -- ListType with typevar inside!
  ti env meta (TupleLit (e1, e2)) = do
    (s1, t1) <- ti env meta e1
    (s2, t2) <- ti env meta e2
    return (s1 `composeSubst` s2, TupleType t1 t2)

-- Check if the type of 2 expr can be CheckType and then return ResultType
--   ResultType -> CheckType -> ...
tcBinOp :: SourceSpan -> Type -> Type -> (FunEnv, VarEnv) -> Expr ReturnsCheckedP  -> Expr ReturnsCheckedP  -> TI (Subst, Type)
tcBinOp meta resultT checkT env e1 e2 = do
  s1 <- tc env meta e1 checkT
  s2 <- tc env meta e2 checkT
  return (s1 `composeSubst` s2, resultT)

-- todo Dependent types for booleans

{- Check if the infered types of 2 expr can be unified with eachother and then return ResultType as the type of the 2 expressions
We are checking if the two types of the expressions are equal in a sense -}
tcBinOpEqual :: SourceSpan -> Type -> (FunEnv, VarEnv) -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpEqual meta resultT env e1 e2 = do
  (s1, t1) <- ti env meta e1
  (s2, t2) <- ti env meta e2
  s3 <- unify meta t1 t2
  return (s1 `composeSubst` s2 `composeSubst` s3, resultT)

{- Checks if the types of 2 expressions match the given type and returns the given type as the type of the expression -}
tcBinOpIdentity :: SourceSpan -> Type -> (FunEnv, VarEnv) -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP -> TI (Subst, Type)
tcBinOpIdentity meta t = tcBinOp meta t t

-- Checks if the types of 2 expressions match the given type returns and returns a boolType
tcBinOpBoolean :: SourceSpan -> Type -> (FunEnv, VarEnv) -> Expr ReturnsCheckedP -> Expr ReturnsCheckedP-> TI (Subst, Type)
tcBinOpBoolean meta = tcBinOp meta BoolType

instance Typecheck (Expr ReturnsCheckedP) where
  ti env _ (LiteralExpr meta lit) = ti env meta lit
  ti env _ (BinOpExpr meta Mul e1 e2) = tcBinOpIdentity meta IntType env e1 e2
  ti env _ (BinOpExpr meta Mod e1 e2) = tcBinOpIdentity meta IntType env e1 e2
  ti env _ (BinOpExpr meta Add e1 e2) = tcBinOpIdentity meta IntType env e1 e2
  ti env _ (BinOpExpr meta Div e1 e2) = tcBinOpIdentity meta IntType env e1 e2
  ti env _ (BinOpExpr meta Sub e1 e2) = tcBinOpIdentity meta IntType env e1 e2
  -- These next ones are polymorf, they can either be char or int
  ti env _ (BinOpExpr meta Gt e1 e2) = do
    (_, t1) <- ti env meta e1
    (_, t2) <- ti env meta e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean meta IntType env e1 e2
      `catchError` \_ ->
        tcBinOpBoolean meta CharType env e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to > should be either Int or Char but you gave " ++ you_gave
  ti env _ (BinOpExpr meta Gte e1 e2) = do
    (_, t1) <- ti env meta e1
    (_, t2) <- ti env meta e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean meta IntType env e1 e2
      `catchError` \_ ->
        tcBinOpBoolean meta CharType env e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to >= should be either Int or Char but you gave " ++ you_gave
  ti env _ (BinOpExpr meta Lt e1 e2) = do
    (_, t1) <- ti env meta e1
    (_, t2) <- ti env meta e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean meta IntType env e1 e2
      `catchError` \_ ->
        tcBinOpBoolean meta CharType env e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to < should be either Int or Char but you gave " ++ you_gave
  ti env _ (BinOpExpr meta Lte e1 e2) = do
    (_, t1) <- ti env meta e1
    (_, t2) <- ti env meta e2
    let you_gave = show t1 ++ " > " ++ show t2
    tcBinOpBoolean meta IntType env e1 e2
      `catchError` \_ ->
        tcBinOpBoolean meta CharType env e1 e2
          `catchError` \_ -> throwError $ "Invalid operation at " ++ show meta ++ "\nArguments to <= should be either Int or Char but you gave " ++ you_gave
  ti env _ (BinOpExpr meta Eq e1 e2) = tcBinOpEqual meta BoolType env e1 e2
  ti env _ (BinOpExpr meta Neq e1 e2) = tcBinOpEqual meta BoolType env e1 e2
  ti env _ (BinOpExpr meta And e1 e2) = tcBinOpIdentity meta BoolType env e1 e2
  ti env _ (BinOpExpr meta Or e1 e2) = tcBinOpIdentity meta BoolType env e1 e2
  ti env _ (BinOpExpr meta Cons e1 e2) = do
    (s1, t1) <- ti env meta e1
    (s2, t2) <- ti env meta e2
    -- Maybe we should see if we can tc with a list that has a type var inside
    s3 <- case t2 of
            -- If t2 is a list type we have to unify with the type inside the list
            ListType u1 -> unify meta t1 u1  `catchError` \err -> throwError $ "You tried to cons " ++ show t1 ++ " with " ++ show t2 ++ ", but this is not legal.\n" ++ err
            -- Else just try to unify and let it fail (most likely)
            _ -> unify meta t1 t2
    -- Todo ask is this needed?
    -- newTy <- newTyVar
    -- s4 <- tc env meta e2 (ListType newTy) 
    let s = s1 `composeSubst` s2 `composeSubst` s3 --`composeSubst` s4
    return (s, ListType t1) -- This is good because we always get a list type like this and we are sure we can put t1 inside it 
  ti env _ (UnaryOpExpr meta Negate e) = do
    (s1, t) <- ti env meta e
    s2 <- unify meta BoolType t -- Check if its a bool, again here we could actually negate the bool maybe, like dependent types but only for bools?
    return (s1 `composeSubst` s2, t)
  ti _env _ (UnaryOpExpr _ (FieldAccess _field) _expr) = undefined
  -- Todo Variables can have fields but I am just going to take the type of the String, I am leaving the warning
  ti (_, varenv) _ (VariableExpr meta (Identifier var _field)) = case Map.lookup var varenv of
    Nothing -> throwError $ "Unbound variable: '" ++ blue var ++ "' at " ++ showStart meta
    Just sigma -> pure (nullSubst, sigma)
  ti (funenv, _) _ (FunctionCallExpr meta funcName _args) = do
    let schemeM = Map.lookup funcName funenv
    scheme <- case schemeM of
      Nothing -> throwError $ "Function " ++ funcName ++ " undefined. at " ++ showStart meta
      Just scheme -> return scheme
    -- If we get here we did find the function!
    retType <- instantiate scheme
    let _ = case retType of
          FunType {} -> error $ "Got a funtype after instantiate scheme, how does that happen!!!!" ++ showStart meta
          -- a -> a
          {- FunType argsT rT -> do
            s1 <- zipWithM (tc env meta) args argsT
            let s = foldr composeSubst nullSubst s1
            return (s, apply s rT) -}
          _ -> error $ "Function environment contains something weird " ++ show retType ++ "!!!!!" ++ showStart meta
    return (nullSubst, retType)

-- Maybe we also do this for list of expressions but probably not because a list of expressions does not make sense
instance (Typecheck a) => Typecheck [a] where
  ti env meta l = do
    subs_ty <- mapM (ti env meta) l
    let subs = map fst subs_ty
    let s = foldr composeSubst nullSubst subs
    return (s, VoidType)
  tc = error "Tc does not make sense yet for lists of things idk good luck with it"

-- This could also be different version of ti in the typeclass that you don't have to implement??

instance Typecheck (Stmt ReturnsCheckedP) where
  ti env _ (ReturnStmt meta (Just e)) = ti env meta e
  -- Here we can check maybe the return type of the function?
  -- This has to be a funtion that checks the type of the return for the function we are in
  -- This should be in the env I think, just like meta
  ti (_, _) _ (ReturnStmt _ Nothing) = return (nullSubst, VoidType)
  ti env _ (IfStmt meta cond consequent (Just alternative)) =
    do
      s1 <- tc env meta cond BoolType
      (s2, _) <- ti env meta consequent
      (s3, _) <- ti env meta alternative
      let s = s1 `composeSubst` s2 `composeSubst` s3
      return (s, VoidType)
  ti env _ (IfStmt meta cond consequent Nothing) =
    do
      s1 <- tc env meta cond BoolType
      (s2, _) <- ti env meta consequent
      let s = s1 `composeSubst` s2
      return (s, VoidType)
  ti env@(_, varenv) _ (AssignStmt meta (Identifier var _) expr) = do
    (s, t) <- ti env meta expr
    let varTM = Map.lookup var varenv
    varT <- case varTM of
      Nothing -> throwError $ "Undefined variable " ++ show var ++ " at " ++ show meta ++ "."
      Just varT -> return varT
    s' <- unify meta varT t
    return (s `composeSubst` s', varT)
  ti env _ (WhileStmt meta cond stmts) =
    do
      s1 <- tc env meta cond BoolType
      (s2, _) <- ti env meta stmts
      let s = s1 `composeSubst` s2
      return (s, VoidType)
  ti env _ (ExprStmt meta e) = ti env meta e
  -- ti env meta (BlockStmt stmt) = Debug.trace "ti called on BlackStmt!! That should not happen right?" ti env meta stmt
  ti _env _meta (BlockStmt _body) = error "ti called on BlackStmt!! That should not happen right during type checking?"

-- todo remove the debug.trace

{-

The problem with this is that we don't need to actually combine the subs from these but if one is wrong we need to know it.
  OHHHH We return a fun type ofcourse!! From that we can upgrade the decl in the wrapper function!

-}
instance Typecheck (Decl ReturnsCheckedP) where
  ti env _ (VarDecl meta _name _ expr) = ti env meta expr
  -- Return the return type?
  ti (funenv, varenv) _ (FunDecl meta name retty args fundelcs body) = do
      -- Add the args to the varenv
      _ <- Debug.trace (blue ("Inferencing function '" ++ name ++ "': ")) pure ()
      -- _ <- Debug.trace ("CheckFunctions' got funenv: " ++ show funenv) pure ()
      -- _ <- Debug.trace ("Building arg env from args: " ++ show args) pure ()
      argenv <-  foldM insertFunArgsIntoEnv Map.empty args
      let varenvWithArgs = argenv <> varenv -- I hope this does not break :()
      return_type <- maybe newTyVar pure retty
      -- lets check the funvars
      -- Do this before adding the current function to the fun env. This prevents an infinite loop, todo add a better error message for this
      _ <- either throwError pure (checkDuplicateVarDecl fundelcs)
      _ <- Debug.trace ("Adding fundecl to varenv':\n" ++ prettyPrintMap varenvWithArgs) pure ()
      (funvarsub, (funenvAfterAddingVarDelcs,varenvWithArgsAndVarDecls)) <- buildVarEnv fundelcs (funenv, varenvWithArgs) 
          -- Add a typescheme fun to the fun var
      let argnames = map fst args
          funenvWithMe = Map.insert name (Scheme argnames return_type) funenvAfterAddingVarDelcs
      _ <- Debug.trace ("Added fun to to funenv':\n" ++ prettyPrintMap funenvWithMe) pure ()

      -- These are the envs to use moving forward
      _ <- Debug.trace ("checking body with fun env:\n" ++ prettyPrintMap funenvWithMe ++ " var env:\n" ++ prettyPrintMap varenvWithArgsAndVarDecls) pure ()
      let final_env = (funenvWithMe, varenvWithArgsAndVarDecls)
      (bodysub, _) <- ti final_env meta body
      -- _ <- Debug.trace ("Got Subs " ++ show body_ti) pure ()
      -- let bodysub = foldr (composeSubst . fst) nullSubst body_ti
      -- hi <- map (\statement -> ti (funenv', varenv'') meta statement) body

      -- Idk if we have to compose these subs here??
      let function_sub = bodysub `composeSubst` funvarsub `composeSubst` varenvWithArgsAndVarDecls
          args' = updateFunArgs argnames function_sub
          -- We need a list of types for the new fundecls 
          argTypes = map snd args'
      fundecls' <- mapM (varDeclsToType function_sub) fundelcs
        -- We do use the latest funenv I think
      return (function_sub, FunType argTypes fundecls' return_type)

        where varDeclsToType :: VarEnv -> Decl ReturnsCheckedP -> TI Type
              varDeclsToType _ (FunDecl {}) = error "We don't support nested fundecls yet"
              varDeclsToType env (VarDecl _ name _ _) = case Map.lookup name env of
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
buildVarEnv (VarDecl meta name (Just specified_type) expr : ds) env@(funenv, varenv) = do
  s1 <- tc env meta expr specified_type

  (s2, env') <- buildVarEnv ds (funenv, Map.insert name specified_type varenv)
  return (s1 `composeSubst` s2, env')
-- If they did not give a type then just infer it :)
buildVarEnv (VarDecl meta name Nothing expr : ds) env@(funenv, varenv) = do
  (s1, ty) <- ti env meta expr
  (s2, env') <- buildVarEnv ds (funenv, Map.insert name ty varenv)
  return (s1 `composeSubst` s2, env')

formatUnifyError :: SourceSpan -> [Char]
formatUnifyError meta = " at " ++ showStart meta ++ " until line " ++ (case endPos meta of SourcePos _ b a -> show (unPos b) ++ " column " ++ show (unPos a)) ++ "\n\nLearn more about unification errors here: https://en.wikipedia.org/wiki/Unification_(computer_science) or here https://cloogle.org/#unify%20error"

-- todo fix list stmt nicer code

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
      (sub, env'', program) <- checkFunctions' rest env'
      -- This should always be Just because its a global var
      let ty = fromMaybe (varDeclTypeNotFoundError name varenv) maybetype
          typedD = upgrade d -- this will break if we add types to the meta of expresssions
      return (sub, env'', VarDecl info name ty typedD : program)

    -- FunDecl
    -- FunDeclT ParsedP = Maybe Type
    checkFunctions' ((FunDecl meta name ty args funvars body) : rest_program) (funenv, varenv) = do
      -- Add the args to the varenv
      _ <- Debug.trace (blue ("Checking function '" ++ name ++ "': ")) pure ()
      -- _ <- Debug.trace ("CheckFunctions' got funenv: " ++ show funenv) pure ()
      -- _ <- Debug.trace ("Building arg env from args: " ++ show args) pure ()
      argenv <-  foldM insertFunArgsIntoEnv Map.empty args
      let varenv' = argenv <> varenv -- I hope this does not break :()
      _ <- Debug.trace ("Varenv' at end of function:\n" ++ prettyPrintMap varenv') pure ()
      return_type <-  maybe newTyVar pure ty
      let argnames = map fst args
          -- Add a typescheme fun to the fun var

          funenv' = Map.insert name (Scheme argnames return_type) funenv
      -- lets check the funvars
      -- todo what if you have duplicate vars in your
      _ <- either throwError pure (checkDuplicateVarDecl funvars)
      -- and also add the fun vars to the varenv
      _ <- Debug.trace ("Adding fundecl to varenv':\n" ++ prettyPrintMap varenv') pure ()
      _ <- Debug.trace ("Added fun to to funenv':\n" ++ prettyPrintMap funenv') pure ()

      (funvarsub, env') <- buildVarEnv funvars (funenv', varenv')

      _ <- Debug.trace ("checking body with fun env" ++ prettyPrintMap funenv' ++ " var env:" ++ prettyPrintMap (snd env')) pure ()

      body_ti <- mapM (ti env' meta) body
      -- _ <- Debug.trace ("Got Subs " ++ show body_ti) pure ()
      let bodysub = foldr (composeSubst . fst) nullSubst body_ti
      -- hi <- map (\statement -> ti (funenv', varenv'') meta statement) body

      {- Apply the substition to the varenv''-}
      let (funenv'', varenv'') = env'
      let function_sub = bodysub `composeSubst` funvarsub `composeSubst` varenv''

      let args' = updateFunArgs argnames function_sub
          funvars' = mergeTypesFunvars funvars function_sub
          body' = map upgrade body
      -- I think we need to forget the variables we added to the varenv!
      -- Maybe even the function_sub??

      _ <- Debug.trace ("Checking next function with function env: " ++ show funenv'' ++ "\nand og var env: " ++ show varenv) pure ()
      (sub, env'', checked_program) <- checkFunctions' rest_program (funenv'', varenv)
      -- I think we maybe now can look up the return type of this function in env
      -- Idk if we have to compose these subs here??
        -- We do use the latest funenv I think
      return (sub, env'', FunDecl meta name return_type args' funvars' body' : checked_program)
    -- how do we make a type scheme?

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

-- We use this for global vras and in the function decl type checking so because of that the return type checking has to go first
checkDuplicateVarDecl :: Program ReturnsCheckedP -> Either String String
checkDuplicateVarDecl = checkDuplicateVarDecl' Map.empty
  where
    checkDuplicateVarDecl' _ [] = Right "No duplicate declerations"
    checkDuplicateVarDecl' env (FunDecl {} : program) = checkDuplicateVarDecl' env program
    checkDuplicateVarDecl' env ((VarDecl meta name _ _) : program) =
      case Map.lookup name env of
        Nothing -> checkDuplicateVarDecl' (Map.insert name meta env) program
        Just meta' -> Left $ "\nVariable with name "
                              ++ name
                              ++ " defined two times!\n"
                              ++ "The first time at: "
                              ++ showStart meta'
                              ++ "\n"
                              ++ "The second time at: "
                              ++ showEnd meta
                              ++ "\n"

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
