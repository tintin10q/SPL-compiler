{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module SPL.Codegen.GenSSM where

import SPL.AST 

import SPL.Codegen.SSM
import Data.Char (ord)
import qualified Data.Map as Map
-- import Control.Monad.Reader
import Control.Monad.State.Lazy

data Location = Adress Int Type | LocalVar Int Type
              deriving (Eq, Ord)
data Key = Fun String | Var String
              deriving (Eq, Ord)

-- Map from a variable name to the code required to get the value on the stack
type Info = (Map.Map Key Location) 
type Env = State Info
-- If function arguments overwrite this at the start of a function call then that is ok you just loose access to the global value.


class GenSSM a where
    generate :: a -> Env Code

instance Semigroup (Env Code) where
      m1 <> m2 = m1 >>= (\code -> m2 >>= \code2 -> pure $ code <> code2)

instance Monoid (Env Code) where
      mempty = pure []

-- Hoe gaan we de variables enviroment doen?
-- Volgens mij hoeven we niet een enviroment voor functies te maken
-- Mischien 1 map voor vars van String naar code om het op de stack te krijgen, 
--  in een functie overwrite een arg met dezelde naam de globals als het shadowed

data VarData = VarData {
                      updateCode :: Code, -- Code to update value we find on stack in the storage,
                      loadCode :: Code, -- Code to push variable onto the stack
                      allocatedLength :: Int,  -- Length of the variable
                      name :: String, -- name of the variabe 
                      offset :: Int, -- Ofset in the heap
                      typeof :: Type } -- Type of the variable



-- Generate INLINE code that prints a runtime error and then halts.
-- Maybe we could also include this in the runtime and then we jump to it instead
throw :: String -> Code
throw error = [LDS i | i <- chars] <> [TRAP 1 | _ <- chars] <> [HALT]
              where chars = map ord error


-- We have a global value
-- First we need to get the value onto the stack, we do this by running the code of the expression
-- After that we have the value on the stack
-- We can store it into the heap
-- When we store it into the heap we can save the offset 


-- We can make R7 store the initial heap value
-- Generates a function that generates the code needed to store and update a value on the head
-- The generated update code assumes the value to save is on the stack before the update code. 
-- The value will be consumed
genSaveGlobalCode :: Decl TypecheckedP -> (Int -> VarData)
genSaveGlobalCode (VarDecl _ name t e) = case t of
                                          IntType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset IntType
                                          CharType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset CharType
                                          BoolType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset BoolType
                                          _ -> undefined
genSaveGlobalCode _ = undefined


emptyEnv :: Info
emptyEnv = Map.empty

getSmmCode :: Program TypecheckedP -> Code
getSmmCode p = let (code, env) = runState (generate p) emptyEnv in code

-- With the tuples we should probably have a pointer to the tuple somewhere and then based on the field access load the value.

-- Cons will be difficult! How? We have to grow the list in the heap.
-- We need to think properly first about how we will store all this stuff

-- Global variables should be on the heap with a way to somehow grow lists. 
-- At least the types of variables will stay the same. 
-- So besides lists getting bigger the size of a variable stays the same.

--The hard part is that things are not the same size.
-- Maybe we return these records with a bunch of info for the global variables.
-- And in that record we put an update function to update the value of the global on the heap

--- How do we have growing lists on the stack? You can keep adding to a list with a while loop
-- How do we have growing things? they did not explain that at all

-- At the moment there is actually no direct list index.
-- So you can only find an item in a list linearly maybe that makes code generation easier. 

instance GenSSM (Program TypecheckedP) where
      generate program = pure [HALT, HALT]
      {-
      generate program = let
                        varDecls = filter isFunction program
                        funDecls = filter isFunction program
                        storeGlobalVarOnHeapCode = foldMap generate varDecls
                        programCode = foldMap generate funDecls
                        in storeGlobalVarOnHeapCode <> programCode
                  where isFunction f = case f of  {FunDecl {} -> True; VarDecl {} -> False}
                        isVarDecl = not . isFunction
                        -}


instance GenSSM (Stmt TypecheckedP) where
      generate _ = undefined

-- we could reverses the code 

instance GenSSM BinOp where
      generate  Mul = pure [MUL]
      generate Div = pure [DIV]
      generate Mod = pure [MOD]
      generate Add = pure [ADD]
      generate Sub = pure [SUB]
-- For (:) (cons), the stack looks like this:
--
-- |  value  |
-- |   addr  |
-- |  .....  |
--
-- The application of the cons operation should
-- put (value, addr) on the heap, and put the
-- address back on the stack at the top.
      generate Cons = pure [STMH 2]
      generate Gt = pure [SPL.Codegen.SSM.GT]
      generate Gte = pure [GE]
      generate Lt = pure [SPL.Codegen.SSM.LT]
      generate Lte = pure [LE]
      generate Eq = pure [SPL.Codegen.SSM.EQ]
      generate Neq = pure [NE]
      generate And = pure [AND]
      generate Or = pure [OR]

instance GenSSM UnaryOp where
      generate Negate = pure [NEG]
      -- TODO: Fix generate for FieldAccess
      generate _ = error "Field access is not supported. :("

instance GenSSM (Expr TypecheckedP) where
      generate (BinOpExpr _ op left right) = generate left <> generate right <> generate op
      generate (UnaryOpExpr _ op operand) = generate  operand <> generate op
      generate (FunctionCallExpr meta func args) = pure [LINK $ length args]
      generate (VariableExpr _ variable) = undefined
      generate (LiteralExpr meta literal) = generate literal

-- We can generate the enviroment before we do any code generation because we know where it will end up
-- buildFunEnv :: [Decl TypecheckedP] -> Env Code -> Env Code
-- buildFunEnv decls env = env <> Map.fromList (zip (map nameToVarKey decls) ([LocalVar i | i <- [1..]] <*> map getDeclType decls))
      -- where nameToVarKey (VarDecl _ name _ _) = Var name
            -- nameToVarKey (FunDecl _ name _ _ _ _) = Fun name

-- Ok laten we eerst maar gewoon even 5 keer een nop of halt genereren ofzo

instance GenSSM (Decl TypecheckedP) where
      generate :: Decl TypecheckedP -> Env Code
      generate  (FunDecl _ name ty [] funvardecl body) = do
            inital <- get
            let funvardecls = map (generate . getExpr) funvardecl
            put inital
            return [HALT, HALT]
            
            -- No arguments 

            -- declCode <- local (buildFunEnv funvardecl)
            -- a <- asks $ Map.lookup (Var "hi")


            -- return declCode
            -- return $ pure (LINK $ length args) ++ declCode
                  -- where getExpr (VarDecl _ _ _ expr) = expr
                        -- getExpr (FunDecl {}) = error "It does not make sense to get the expression of a fun decl"

      generate (VarDecl _ name _ expr)  = return [HALT, HALT]
      generate  (FunDecl _ name ty args funvardecl body) = error "no argument support yet"


getExpr :: Decl TypecheckedP -> Expr TypecheckedP
getExpr (VarDecl _ name _ expr)  = expr
getExpr f@(FunDecl _ _ _ _ _ body) = error $ "Can only get expressions from vardecl not fun delc" ++ show f

combineCode :: [Env Code] -> Env Code
combineCode [] = pure [] -- terrible and slow
combineCode (re:rest) = re >>= \code -> combineCode rest >>= \code' -> pure $ code ++ code'

instance GenSSM (Literal TypecheckedP) where
      generate TrueLit = pure [LDS 1] -- There is also a True and False but its just a bit pattern https://webspace.science.uu.nl/~hage0101/SSM/ssmtopics.html#True
      generate FalseLit = pure [LDS 0]
      generate (IntLit int)  = pure [LDS int]
      generate (CharLit char)  = pure [LDS $ ord char] -- Here we forget that it was a char
      generate (TupleLit (e1, e2)) = pure [LDH 0] <> generate e1
                                          <> pure [LDH 1] <> generate e2 -- Wrong but yeah maybe store in heap?
      generate EmptyListLit = pure [LDC 0, STH]