-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- module SPL.Typechecker where

-- import SPL.Parser.AST
-- import SPL.Parser.Parser (SourceSpan)
-- import Data.List

-- type instance FunDecl InstantiatedP = SourceSpan
-- type instance FunDeclT InstantiatedP = Type
-- type instance VarDecl InstantiatedP = SourceSpan
-- type instance VarDeclT InstantiatedP = Type

-- deriving instance Eq (Decl InstantiatedP)
-- deriving instance Show (Decl InstantiatedP)

-- type instance ReturnStmt InstantiatedP = SourceSpan
-- type instance IfStmt InstantiatedP = SourceSpan
-- type instance WhileStmt InstantiatedP = SourceSpan
-- type instance ExprStmt InstantiatedP = SourceSpan
-- type instance VarStmt InstantiatedP = SourceSpan

-- deriving instance Eq (Stmt InstantiatedP)
-- deriving instance Show (Stmt InstantiatedP)

-- type instance BinOpExpr InstantiatedP = SourceSpan
-- type instance UnaryOpExpr InstantiatedP = SourceSpan
-- type instance AssignExpr InstantiatedP = SourceSpan
-- type instance FunctionCallExpr InstantiatedP = SourceSpan
-- type instance VariableExpr InstantiatedP = SourceSpan
-- type instance LiteralExpr InstantiatedP = SourceSpan

-- deriving instance Eq (Expr InstantiatedP)
-- deriving instance Show (Expr InstantiatedP)

-- deriving instance Eq (Literal InstantiatedP)
-- deriving instance Show (Literal InstantiatedP)

-- type instance FunDecl TypecheckedP = SourceSpan
-- type instance VarDecl TypecheckedP = SourceSpan

-- type instance ReturnStmt TypecheckedP = SourceSpan
-- type instance IfStmt TypecheckedP = SourceSpan
-- type instance WhileStmt TypecheckedP = SourceSpan
-- type instance ExprStmt TypecheckedP = SourceSpan
-- type instance VarStmt TypecheckedP = SourceSpan

-- type instance BinOpExpr TypecheckedP = (SourceSpan, Type)
-- type instance UnaryOpExpr TypecheckedP = (SourceSpan, Type)
-- type instance AssignExpr TypecheckedP = (SourceSpan, Type)
-- type instance FunctionCallExpr TypecheckedP = (SourceSpan, Type)
-- type instance VariableExpr TypecheckedP = (SourceSpan, Type)
-- type instance LiteralExpr TypecheckedP = (SourceSpan, Type)

-- instance Convertable Stmt ParsedP InstantiatedP where
--   convert (ReturnStmt pos expr) =
--     ReturnStmt pos (convert <$> expr)
--   convert (IfStmt pos condition consequent alternative) =
--     IfStmt pos (convert condition) (convert <$> consequent) (map convert <$> alternative)
--   convert (WhileStmt pos condition body) =
--     WhileStmt pos (convert condition) (convert <$> body)
--   convert (ExprStmt pos expr) =
--     ExprStmt pos (convert expr)
--   convert (VarStmt pos ty name expr) =
--     VarStmt pos ty name (convert expr)

-- instance Convertable Expr ParsedP InstantiatedP where
--   convert (BinOpExpr pos op left right) =
--     BinOpExpr pos op (convert left) (convert right)
--   convert (UnaryOpExpr pos op expr) =
--     UnaryOpExpr pos op (convert expr)
--   convert (AssignExpr pos x expr) =
--     AssignExpr pos x (convert expr)
--   convert (FunctionCallExpr pos name args) =
--     FunctionCallExpr pos name (convert <$> args)
--   convert (VariableExpr pos x) =
--     VariableExpr pos x
--   convert (LiteralExpr pos lit) =
--     LiteralExpr pos (convert lit)

-- instance Convertable Literal ParsedP InstantiatedP where
--   convert TrueLit = TrueLit
--   convert FalseLit = FalseLit
--   convert (IntLit n) = IntLit n
--   convert (CharLit c) = CharLit c
--   convert (TupleLit (l, r)) = TupleLit (convert l, convert r)
--   convert EmptyListLit = EmptyListLit

-- type Bookkeeping = [String]
-- type Context = [(String, Type)]

-- -- Generate a fresh variable based on the given namespace "x" and
-- -- the already generated free variables "vs".
-- freshVar :: String -> [String] -> String
-- freshVar x vs = x ++ show (length $ filter (isPrefixOf x) vs)

-- -- Instantiates a type.
-- instantiateType
--   :: String                               -- A fresh type variable
--   -> Bookkeeping                          -- Bookkeeping for generating new *unique* type variables
--   -> Context                              -- The current context of type variables to fresh type variables
--   -> Maybe Type                           -- The type to instantiate
--   -> (Bookkeeping, Context, Type)         -- A tuple containing (1) the new list of generated variables
--                                           --                    (2) the new context
--                                           --                    (3) the assigned type
-- instantiateType fv bookkeeping ctx t = case t of
--   -- In case of Nothing, generate a completely fresh type variable
--   Nothing -> (fv:bookkeeping, ctx, TypeVar fv)
--   -- In case of a type variable,
--   --  * check if we have a type for it, 
--   --  * otherwise generate one
--   Just (TypeVar v) ->
--     case lookup v ctx of
--       -- We have a type for it
--       Just t' -> (bookkeeping, ctx, t')
--       -- We need to generate a fresh type variable
--       Nothing -> (fv:bookkeeping, (v, TypeVar fv):ctx, TypeVar fv)
--   -- In case of a fully annotated type, use that directly without modifying the context
--   Just t' -> (bookkeeping, ctx, t')

-- instantiate :: Program ParsedP -> Program InstantiatedP
-- instantiate = instantiate' [] []
--   where
--     instantiate'
--       :: Bookkeeping        -- Bookkeeping for generating new *unique* type variables
--       -> Context            -- The context of known type variables
--       -> Program ParsedP    -- The declarations to generate an environment for
--       -> Program InstantiatedP
--     instantiate' _ _ [] = []
--     instantiate' bookkeeping ctx (decl:decls) = case decl of
--       VarDecl loc name Nothing expr ->
--         let fv = freshVar "g" bookkeeping 
--             instantiatedVar = VarDecl loc name (TypeVar fv) (convert expr)
--         in instantiatedVar : instantiate' (fv : bookkeeping) ctx decls
--       VarDecl loc name (Just ty) expr ->
--         -- If the variable has a specified type, use that directly
--         VarDecl loc name ty (convert expr) : instantiate' bookkeeping ctx decls
--       FunDecl loc name returnTy args body ->
--         -- Annotate the type for the return type first.
--         let (bookkeeping', ctx', instantiatedReturnTy) = instantiateType (freshVar "f" bookkeeping) bookkeeping ctx returnTy
--             -- Using the new bookkeeping (bookkeeping') and context (ctx'), annotate the arguments
--             (bookkeeping'', _, instantiatedArgs) = instantiateArgs bookkeeping' ctx' args
--             -- Update the FunDecl to use the annotated types
--             instantiatedFun = FunDecl loc name instantiatedReturnTy instantiatedArgs (convert <$> body)
--         -- Recursively annotate the other functions using the newest bookkeeping (bookkeeping'') with a fresh (empty) context
--         in instantiatedFun : instantiate' bookkeeping'' [] decls
--         where
--           instantiateArgs
--             :: Bookkeeping                                    -- Bookkeeping for generating new *unique* type variables
--             -> Context                                        -- The context of known type variables 
--             -> [(String, Maybe Type)]                         -- The arguments to annotate
--             -> (Bookkeeping, Context, [(String, Type)])       -- A tuple containing (1) the new list of generated variables
--                                                               --                    (2) the new context
--                                                               --                    (3) the annotated args
--           instantiateArgs bookkeeping ctx [] = (bookkeeping, ctx, [])
--           instantiateArgs bookkeeping ctx ((argName, ty):args') =
--             -- Annotate the first argument
--             let (bookkeeping', ctx', instantiatedTy) = instantiateType (freshVar "f" bookkeeping) bookkeeping ctx ty
--                 -- Recursively annotate the next arguments using the new bookkeeping (bookkeeping') and new context (ctx')
--                 (bookkeeping'', ctx'', annotatedArgs) = instantiateArgs bookkeeping' ctx' args'
--             -- Return the newest bookkeeping (bookkeeping'') and newest context (ctx'') and the annotated arguments
--             in (bookkeeping'', ctx'', (argName, instantiatedTy):annotatedArgs)

-- -- typecheck :: Program ParsedP -> Either String (Program TypecheckedP)
-- -- typecheck parsedProgram = case instantiatedProgram of
-- --   [] -> Right []
-- --   (decl:decls) -> undefined
-- --   where instantiatedProgram = instantiate parsedProgram

-- infer :: Context -> Program InstantiatedP -> Either String (Context, Program TypecheckedP)
-- infer ctx [] = Right (ctx, [])
-- infer ctx (p:ps) = do
--   (ctx', p') <- inferDecl ctx p
--   (ctx'', ps') <- infer ctx' ps
--   return (ctx'', p':ps')

-- inferDecl :: Context -> Decl InstantiatedP -> Either String (Context, Decl TypecheckedP)
-- inferDecl ctx (VarDecl loc name ty expr) = undefined
-- inferDecl ctx (FunDecl loc name returnTy args body) = undefined

-- inferExpr :: Program InstantiatedP -> Context -> Expr InstantiatedP -> Either String (Context, Type)
-- inferExpr p ctx (BinOpExpr pos op left right) = undefined
-- inferExpr p ctx (UnaryOpExpr pos op expr) = undefined
-- inferExpr p ctx (AssignExpr pos var expr) = undefined
-- inferExpr p ctx (FunctionCallExpr pos name body) = undefined
-- inferExpr p ctx (VariableExpr pos var) = undefined
-- inferExpr p ctx (LiteralExpr pos lit) = undefined

-- inferLit :: Program InstantiatedP -> Context -> Literal InstantiatedP -> Either String (Context, Type)
-- inferLit _ ctx TrueLit = Right (ctx, BoolType)
-- inferLit _ ctx FalseLit = Right (ctx, BoolType)
-- inferLit _ ctx (IntLit _) = Right (ctx, IntType)
-- inferLit _ ctx (CharLit _) = Right (ctx, CharType)
-- inferLit p ctx (TupleLit (left, right)) = do
--   (ctx', left') <- inferExpr p ctx left
--   (ctx'', right') <- inferExpr p ctx' right
--   return (ctx'', TupleType left' right')  
-- inferLit _ ctx EmptyListLit = undefined


-- unify :: Type -> Type -> Either String Context
-- unify IntType IntType = Right []
-- unify CharType CharType = Right []
-- unify BoolType BoolType = Right []
-- unify (TypeVar x) (TypeVar y)
--   | x == y = Right []
--   | otherwise = Left $ "Cannot unify the type variables " ++ x ++ " and " ++ y ++ "."
-- unify (TypeVar x) t = if x `elem` tv t
--   then Left "Occurs check: cannot construct the infine type."
--   else Right [(x, t)]
-- unify t (TypeVar x) = if x `elem` tv t
--   then Left "Occurs check: cannot construct the infine type."
--   else Right [(x, t)]
-- unify (ListType t1) (ListType t2) = unify t1 t2
-- unify (TupleType s1 s2) (TupleType t1 t2) = do
--   u <- unify s1 t1
--   unify (subst u s2) (subst u t2)
-- unify x y = Left $ "Cannot unify " ++ show x ++ " with " ++ show y ++ "."

-- subst :: Context -> Type -> Type
-- subst [] t = t
-- subst (x:xs) t = subst xs (substSingle t)
--   where
--     substSingle (TupleType t1 t2) = TupleType (substSingle t1) (substSingle t2)
--     substSingle (ListType t') = ListType (substSingle t')
--     substSingle (TypeVar v) = case x of
--       (v', t') -> if v == v' then t' else TypeVar v
--     substSingle t' = t'

-- -- All the (free) type variables in the given type.
-- tv :: Type -> [String]
-- tv (TypeVar x) = [x]
-- tv (TupleType t1 t2) = tv t1 ++ tv t2
-- tv (ListType t) = tv t
-- tv _ = []


