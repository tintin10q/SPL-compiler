{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module SPL.Codegen.GenSSM where

import SPL.Parser.AST

import SPL.Codegen.SSM
import Data.Char
import qualified Data.Map as Map
import SPL.Typechecker2() -- Import only the type family instances

class GenSSM a where
    generate :: VarEnv -> a -> Code


-- Hoe gaan we de variables enviroment doen?
-- Volgens mij hoeven we niet een enviroment voor functies te maken
-- Mischien 1 map voor vars van String naar code om het op de stack te krijgen, 
--  in een functie overwrite een arg met dezelde naam de globals als het shadowed

data VarData = VarData {
                        updateCode :: Code, -- Code to update value we find on stack in the storage,
                        loadCode :: Code, -- Code to push variable onto the stack
                        length :: Int,  -- Length of the variable
                        allocatedLength :: Int,  -- Length of the variable
                        name :: String, -- name of the variabe 
                        offset :: Int, -- Ofset in the heap
                        typeof :: Type } -- Type of the variable

-- Map from a variable name to the code required to get the value on the stack
type VarEnv = Map.Map String VarData
-- If function arguments overwrite this at the start of a function call then that is ok you just loose access to the global value.

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
genSaveGlobalCode (VarDecl _ name t e) = case fst t of 
                                            IntType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 1 0 name offset IntType
                                            CharType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 1 0 name offset CharType
                                            BoolType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 1 0 name offset BoolType
                                            _ -> undefined
genSaveGlobalCode _ = undefined

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
  generate env program = let
                         varDecls = filter isFunction program
                         funDecls = filter isFunction program
                         storeGlobalVarOnHeapCode = foldMap (generate env) varDecls
                         programCode = foldMap (generate env) funDecls
                          in storeGlobalVarOnHeapCode <> programCode
                    where isFunction f = case f of  {FunDecl {} -> True; VarDecl {} -> False}
                          isVarDecl = not . isFunction

instance GenSSM (Decl TypecheckedP) where
  generate _ decl  = []

instance GenSSM (Stmt TypecheckedP) where
  generate _ _ = []

instance GenSSM BinOp where
  generate _ Mul = [MUL]
  generate _ Div = [DIV]
  generate _ Mod = [MOD]
  generate _ Add = [ADD]
  generate _ Sub = [SUB]
  -- For (:) (cons), the stack looks like this:
  --
  -- |  value  |
  -- |   addr  |
  -- |  .....  |
  --
  -- The application of the cons operation should
  -- put (value, addr) on the heap, and put the
  -- address back on the stack at the top.
  generate _ Cons = [STMH 2]
  generate _ Gt = [SPL.Codegen.SSM.GT]
  generate _ Gte = [GE]
  generate _ Lt = [SPL.Codegen.SSM.LT]
  generate _ Lte = [LE]
  generate _ Eq = [SPL.Codegen.SSM.EQ]
  generate _ Neq = [NE]
  generate _ And = [AND]
  generate _ Or = [OR]

instance GenSSM UnaryOp where
  generate _ Negate = [NEG]
  -- TODO: Fix generate for FieldAccess
  generate _ _ = error "Field access is not supported. :("

instance GenSSM (Expr TypecheckedP) where
  generate env (BinOpExpr _ op left right) = generate env left <> generate env right <> generate env op
  generate env (UnaryOpExpr _ op operand) = generate env operand <> generate env op
  generate env (AssignExpr _ variable expr) = undefined
  generate env (FunctionCallExpr (t, _) func args) = undefined
  generate env (VariableExpr _ variable) = undefined
  generate env (LiteralExpr (t, _) literal) = generate env literal

instance GenSSM (Literal TypecheckedP) where
  generate _ TrueLit = [LDS 1] -- There is also a True and False but its just a bit pattern https://webspace.science.uu.nl/~hage0101/SSM/ssmtopics.html#True
  generate _ FalseLit = [LDS 0]
  generate _ (IntLit int)  = [LDS int]
  generate _ (CharLit char)  = [LDS $ ord char] -- Here we forget that it was a char
  generate env (TupleLit (e1, e2)) = [LDH 0] <> generate env e1
                                    <> [LDH 1] <> generate env e2 -- Wrong but yeah maybe store in heap?
  generate _ EmptyListLit = [LDC 0, STH]