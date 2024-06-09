{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module SPL.Codegen.GenSSM where

import SPL.AST

import SPL.Codegen.SSM
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
-- import Control.Monad.Reader
import Control.Monad.State.Lazy
import Text.Megaparsec (ErrorItem(Label))
import SPL.Parser.SourceSpan (showStart, SourceSpan)
import SPL.Colors (blue, red, black)
import Data.Maybe (fromMaybe)
import SPL.PrettyPrint (Prettier(..))

data Location =  LocalVar  Int Type | GlobalVar Int Type
              deriving (Eq, Ord, Show)
data Key = Fun String | Var String
              deriving (Eq, Ord, Show)

-- Map from a variable name to the code required to get the value on the stack
data Info = Info {
                   genEnv :: Map.Map Key Location, -- genEnv has the data needed to generate code like variables and where they are 
                   globalVarCounter :: Int,
                   needsUnlink :: Bool
                  }

-- Functions defined on Info!

-- Look up a variable from enviroment, doing it in a function gives worse error messages but after type checking we should never error here
getkey :: Key -> Env Location
getkey key = do
            debugEnv
            env <- gets genEnv
            case Map.lookup key env of
                  Nothing -> error $ red "Key called '"++ blue (show key) ++"'" ++ red " not in enviroment!"
                  Just location -> pure location

increaseGlobalVarCount :: Env Int
increaseGlobalVarCount = do
      modify (\env@(Info {globalVarCounter = count}) -> env {globalVarCounter = count + 1})
      gets globalVarCounter

setUnlinkNeeded :: Env ()
setUnlinkNeeded = modify (\env -> env {needsUnlink = True})

setUnlinkNotNeeded :: Env ()
setUnlinkNotNeeded = modify (\env -> env {needsUnlink = False})

logEnv s = Debug.trace (black s) pure ()

debugEnv :: Env ()
debugEnv = do
      (Info env counter needsUnlink) <- get
      logEnv $ "Current gen env: " ++ show env ++ "\nCounter: " ++ show counter ++ "\nNeeds unlink: " ++ show needsUnlink
      return ()


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

-- data VarData = VarData {
--                       updateCode :: Code, -- Code to update value we find on stack in the storage,
--                       loadCode :: Code, -- Code to push variable onto the stack
--                       allocatedLength :: Int,  -- Length of the variable
--                       name :: String, -- name of the variabe 
--                       offset :: Int, -- Ofset in the heap
--                       typeof :: Type } -- Type of the variable



-- Generate INLINE code that prints a runtime error and then halts.
-- Maybe we could also include this in the runtime and then we jump to it instead
throw :: String -> Code
throw error = [LDS i | i <- chars] <> [TRAP 1 | _ <- chars] <> [HALT]
              where chars = map ord error

emptyEnv :: Info
emptyEnv = Info Map.empty 0 False


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

-- instance (GenSSM a) => GenSSM [a] where 
      -- generate a = combineCodeM $ mapM generate 


-- Store Heap pointer into r5
runtime = [LDR HP, STR gvr, Bra "main"] -- Deal with returns from main!

instance GenSSM [Expr TypecheckedP] where
      generate = combineCodeM . mapM generate

instance GenSSM [Stmt TypecheckedP] where
      generate = combineCodeM . mapM generate

instance GenSSM (Program TypecheckedP) where
      generate a = do
            program_code <- combineCodeM $ mapM generate a
            return $ runtime <> program_code
      -- generate program = gnerate program
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

-- Yo jsr looks to be dynamic function calls!

instance GenSSM (Decl TypecheckedP) where
      generate :: Decl TypecheckedP -> Env Code
      generate  (FunDecl _ name VoidType [] [] body) = do
            setUnlinkNotNeeded
            return [ LABEL name ] <>
                   generate body <>
                   pure [if name == "main" then HALT else RET] -- todo Oh implicit returns? 

      -- Put the var decl on the heap, as in at the start save the heap start into a regsiter. When we then want to load vars we 
      --  Load from that register add the offset that we know and load that. 
      -- This only works if we do the var decls first but we do because we hoist them to the top
      generate (VarDecl _ name _ expr)  = do
            -- todo Safe global var
            globalvarcount <- increaseGlobalVarCount
            modify $ \env@(Info {genEnv = genenv}) -> env {genEnv = Map.insert (Var name) (GlobalVar globalvarcount (getType expr)) genenv}
            generate expr <> pure [STH, AJS (-1)] -- todo, this could be stmh if we do all the vardelc first, more safe
                  -- Check if AJS -1 works
      generate  (FunDecl _ name retty args funvardecl body) = do
            -- Load the args and funvardecls into the enviroment
            env@(Info {genEnv = genenv }) <- get
            -- Get the args 
            let argEnv = Map.fromList [(Var argname, LocalVar i argtype) | ((argname, argtype), i) <- zip args [1..]]
                declEnv = Map.fromList [(Var varname, LocalVar i (getType varexpr)) | (VarDecl _ varname _ varexpr, i) <- zip funvardecl [(length argEnv)..]]
            let newGenEnv = genenv <> argEnv <> declEnv
            put env {genEnv = newGenEnv}
            let local_length = length args + length funvardecl
            when (local_length > 0) setUnlinkNeeded
            fundecl_code <- foldMap (\(VarDecl _ varname _ expr) -> generate expr <> storeVar varname) funvardecl
            body_code <- generate body
            put env -- Restore previous env
            -- make space for locals, load the fundecls, the arguments should already be there? 
            return $ [LABEL name] <> [LINK local_length | local_length > 0] <> fundecl_code <> body_code <> [UNLINK | local_length > 0] <> [if name == "main" then HALT else RET] -- The ret here could be better , only add if needed!
            -- todo remove extra return by having phase that explicity adds the implcit return

-- Loading global vars is

loadVar :: String -> Env Code
loadVar name = do
      value <- getkey (Var name)
      case value of
            (GlobalVar offset _) -> return [LDR gvr, LDAA offset]
            (LocalVar offset _) -> return [LDL offset]

-- Returns code to store the value on the head of the stack into a variable
storeVar :: String -> Env Code
storeVar name = do
      value <- getkey (Var name)
      case value of
            (GlobalVar offset _) -> return [LDR gvr, STA offset]
            (LocalVar offset _) -> return [STL offset]


instance GenSSM (Literal TypecheckedP) where
      generate TrueLit = pure [LDC 1] -- There is also a True and False but its just a bit pattern https://webspace.science.uu.nl/~hage0101/SSM/ssmtopics.html#True
      generate FalseLit = pure [LDC 0]
      generate (IntLit int)  = pure [LDC int]
      generate (CharLit char)  = pure [LDC $ ord char] -- Here we forget that it was a char
      generate (TupleLit (e1, e2)) = pure [LDH 0] <> generate e1 <> pure [LDH 1] <> generate e2 -- Wrong but yeah maybe store in heap?
      generate EmptyListLit = pure [LDC 0, STH]

-- Generate an annotate instruction
annotate = Annote SP 0 1 Green

instance GenSSM (Stmt TypecheckedP) where
      generate (ExprStmt _ expr) = generate expr -- Todo clean these up though? Right? 
      generate (AssignStmt _ (Identifier name Nothing) expr) = generate expr <> storeVar name
      generate (AssignStmt _ (Identifier name (Just field)) expr) = error "Field assignment is not supported yet"
      -- Based on the field we have to find out the adress. We can get the adress of locals and also the adress of globals. 
      --  Then we have to dereference them and do get the next adress and store something there!

      generate (ReturnStmt _ (Just e)) = do 
            unlinkNeeded <- gets needsUnlink 
            exprCode <- generate e 
            return $ exprCode <> [STR RR] <> [UNLINK | unlinkNeeded] <> [RET]
      generate (ReturnStmt _ Nothing) = gets needsUnlink >>= \unlink_needed -> pure $ [UNLINK | unlink_needed] <> [RET] -- todo having this kind of forces to always do link even if there is no vars.
      generate (IfStmt _ condition consequent Nothing) = do
                                                 conditionCode <- generate condition
                                                 consequenceCode <- generate consequent
                                                 return $ conditionCode  <> [BRF (codeSize consequenceCode)] <> consequenceCode
      generate (IfStmt _ condition consequent (Just alternative)) = do
                                                 conditionCode <- generate condition
                                                 alternativeCode <- generate alternative
                                                 consequentCode <- generate consequent >>= \code -> pure $ code <> [BRA $ codeSize alternativeCode]
                                                 return $ conditionCode <> [BRF (codeSize consequentCode)] <> consequentCode <> alternativeCode
      generate (WhileStmt _ condition body) = do
            conditionCode <- generate condition
            bodyCode <- generate body -- >>= \bodyCode -> pure $ bodyCode <> [BRA (- (codeSize bodyCode + codeSize conditionCode + 2))] -- +2 for the branch instruction added to condition code
            let branchLength = instrSize (BRF undefined)
                bodyLenght = codeSize bodyCode + branchLength
                conditionLength = codeSize conditionCode + branchLength
                bodyCodeWithBranch = bodyCode ++ [BRA (-(bodyLenght + conditionLength))] -- jump back up to condition
                conditionCodeWithBranch = conditionCode ++ [BRF bodyLenght] -- skip body on false
             in return $ conditionCodeWithBranch <> bodyCodeWithBranch
      generate (BlockStmt list) = foldMap generate list



-- we could reverses the code 

newline :: Int
newline = 10


printStringCode :: String -> Code
printStringCode =  foldMap (\c -> [LDC (ord c), TRAP 1])


instance GenSSM (Expr TypecheckedP) where
      generate (BinOpExpr _ op left right) = generate left <> generate right <> generate op
      generate (UnaryOpExpr _ op operand) = generate  operand <> generate op
      -- Now it would be really nice if we could know the type of arg 
      generate (FunctionCallExpr meta "print" [arg]) = let argType = getType arg in generate arg <> case Debug.trace ("Printing the type " ++ show argType) argType of
                                                                                                      (TypeVar tyname False) -> pure $ printStringCode ("Non rigid TypeVar "++ show tyname ++", idk how to print this!") ++ [LDC newline, TRAP 1]
                                                                                                      (TypeVar tyname True) -> pure $ printStringCode ("Rigid TypeVar"++ show tyname ++", idk how to print this!") ++ [LDC newline, TRAP 1]
                                                                                                      CharType -> pure [TRAP 1, LDC newline, TRAP 1] -- Print adds a newline
                                                                                                      BoolType -> error "Printing bool types is not implemented yet"
                                                                                                      IntType -> pure [TRAP 0, LDC newline, TRAP 1] -- Print adds a newline
                                                                                                      _ -> pure $ printStringCode "Idk how to print this type!" ++ [LDC newline, TRAP 1] -- Print adds a newline
      generate (FunctionCallExpr meta "print" [arg]) = generate arg <> pure [TRAP 0, LDC newline, TRAP 1] -- Print adds a newline
      -- generate (FunctionCallExpr meta "print" (_:_)) = error $ "At " ++ (showStart meta ) + " you call print with too many arguments"
      generate (FunctionCallExpr (ty, _) func args) = do argcode <- generate args
                                                         return $ argcode <> [Bsr func] <> ([LDR RR | ty /= VoidType])

      generate (VariableExpr meta (Identifier varname Nothing)) = loadVar varname
      generate (VariableExpr (ty, _) (Identifier varname (Just _))) = error "Loading fields on variables are unsupported atm!" -- We can know what to do based on the field!
      generate (LiteralExpr meta literal) = generate literal


-- We can generate the enviroment before we do any code generation because we know where it will end up
-- buildFunEnv :: [Decl TypecheckedP] -> Env Code -> Env Code
-- buildFunEnv decls env = env <> Map.fromList (zip (map nameToVarKey decls) ([LocalVar i | i <- [1..]] <*> map getDeclType decls))
      -- where nameToVarKey (VarDecl _ name _ _) = Var name
            -- nameToVarKey (FunDecl _ name _ _ _ _) = Fun name

-- Ok laten we eerst maar gewoon even 5 keer een nop of halt genereren ofzo

getExpr :: Decl TypecheckedP -> Expr TypecheckedP
getExpr (VarDecl _ _ _ expr)  = expr
getExpr f@(FunDecl _ _ _ _ _ body) = error $ "Can only get expressions from vardecl not fun delc" ++ show f


class GetType a where
      getType :: a -> Type

instance GetType (Expr TypecheckedP) where
      getType (BinOpExpr (ty, _) _ _ _) = ty
      getType (UnaryOpExpr (ty, _) _ _) = ty
      getType (VariableExpr (ty, _) _) = ty
      getType (LiteralExpr (ty, _) _) = ty
      getType (FunctionCallExpr (ty, _) _ _) = ty

instance GetType (Decl TypecheckedP)  where
      getType (VarDecl _ _ _ expr)  = getType expr
      getType (FunDecl _ _ retty args _ _) = FunType (map snd args) retty


combineCodeM :: Env [Code] -> Env Code
combineCodeM codes = combineCode <$> codes

combineCode :: [Code] -> Code
combineCode  = foldr (<>) [] -- todo This could be more effiecient with  : and reverse?

instance GenSSM BinOp where
      generate  Mul = pure [MUL]
      generate Div = pure [DIV]
      generate Mod = pure [MOD]
      generate Add = pure [ADD]
      generate Sub = pure [SUB]
      generate Gt = pure [SPL.Codegen.SSM.GT]
      generate Gte = pure [GE]
      generate Lt = pure [SPL.Codegen.SSM.LT]
      generate Lte = pure [LE]
      generate Eq = pure [SPL.Codegen.SSM.EQ]
      generate Neq = pure [NE]
      generate And = pure [AND]
      generate Or = pure [OR]
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


instance GenSSM UnaryOp where
      generate Negate = pure [NEG]
      generate Min = pure [NEG] 
      -- TODO: Fix generate for FieldAccess
      generate _ = error "Field access is not supported. :("


-- We have a global value
-- First we need to get the value onto the stack, we do this by running the code of the expression
-- After that we have the value on the stack
-- We can store it into the heap
-- When we store it into the heap we can save the offset 


-- We can make R7 store the initial heap value
-- Generates a function that generates the code needed to store and update a value on the head
-- The generated update code assumes the value to save is on the stack before the update code. 
-- The value will be consumed
-- genSaveGlobalCode :: Decl TypecheckedP -> (Int -> VarData)
-- genSaveGlobalCode (VarDecl _ name t e) = case t of
                                          -- IntType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset IntType
                                          -- CharType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset CharType
                                          -- BoolType -> \offset -> VarData [LDR R7, STA offset] [LDR R7, LDA offset] 0 name offset BoolType
                                          -- _ -> undefined
genSaveGlobalCode _ = undefined

-- isEmpty is another build in. I guess you just see if its an empty cons cell 