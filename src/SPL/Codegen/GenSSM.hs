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
import SPL.Parser.SourceSpan 
import SPL.Colors (blue, red, black)
import Prelude hiding (EQ)

data Location =  LocalVar  Int Type | GlobalVar Int Type
              deriving (Eq, Ord, Show)
data Key = Fun String | Var String
              deriving (Eq, Ord, Show)

-- Map from a variable name to the code required to get the value on the stack
data Info = Info {
                   genEnv :: Map.Map Key Location, -- genEnv has the data needed to generate code like variables and where they are 
                   globalVarCounter :: Int,
                   needsUnlink :: Bool,
                   needsOutOfBoundsRuntimeExeptionCode :: Bool
                  }

-- Functions defined on Info!

-- Look up a variable from enviroment, doing it in a function gives worse error messages but after type checking we should never error here
getkey :: Key -> Env Location
getkey key = do
            -- debugEnv
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

includeOutOfBoundsRuntimeExceptionCode :: Env ()
includeOutOfBoundsRuntimeExceptionCode = Debug.trace "Needing out of bounds runtime" modify (\env -> env {needsOutOfBoundsRuntimeExeptionCode = True})

replaceGenEnv :: Map.Map Key Location -> Env ()
replaceGenEnv genenv = modify (\env -> env {genEnv = genenv})


logEnvBlack :: Applicative f => [Char] -> f ()
logEnvBlack s = Debug.trace (black s) pure ()
logEnv :: Applicative f => String -> f ()
logEnv s = Debug.trace s pure ()

debugEnv :: Env ()
debugEnv = do
      (Info env counter needUnlink needsOutOfBoundsRuntimeCode) <- get
      logEnv $ black "Current gen env: " ++ show env ++ black "\nCounter: " ++ show counter ++ black "\nNeeds unlink: " ++ show needUnlink ++ black "\nNeeds outOfBound expection runtime code: " ++ show needsOutOfBoundsRuntimeCode
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



-- Generate INLINE code that prints a runtime error and then halts. Only for simple exceptions.
-- For exceptions with arguments include something in the runtime and pop of the stack 
throw :: String -> Code
-- throw msg = [LDS i | i <- chars] <> [TRAP 1 | _ <- chars] <> [HALT] where chars = map ord msg 
throw msg = [LDS 0] <> reverse [LDS i | i <- chars] <> [TRAP 2] <> [HALT] where chars = map ord msg

-- To use this runtime function, first push the column and then the line number. Do not push return adress on the stack as there is no returning from this
outOfBoundExpectionRuntimeCode :: Code
outOfBoundExpectionRuntimeCode = [LABEL "'outOfBoundExpection"] <> printStringCode "\nArray out of bounds runtime expection:\nTrying to access .hd on an empty array. \nCaused by line "
                                 <> [TRAP 0] <> printStringCode " column " <> [TRAP 0, LDC (ord '.'), TRAP 1, HALT]

emptyEnv :: Info
emptyEnv = Info Map.empty (-1) False False


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


notEmpty :: [a] -> Bool
notEmpty = not . null

instance GenSSM [Expr TypecheckedP] where
      generate = combineCodeM . mapM generate

instance GenSSM [Stmt TypecheckedP] where
      generate = combineCodeM . mapM generate

instance GenSSM (Program TypecheckedP) where
      generate program = do

            let varDecls = filter isVarDecl program
                funDecls = filter isFunDecl program
                varDeclCount = length varDecls
            vardecl_code <- foldMap generate varDecls >>= \code -> pure $ code <> if null varDecls then [] else [LDR HP, STR R5, STMH varDeclCount, AJS (-1)]
            fundecls_code <- foldMap generate funDecls                                      -- Not needed as we just enter it
            needOutOfBoundExpectionRuntimeCode <- gets needsOutOfBoundsRuntimeExeptionCode
            return $ vardecl_code <> [Bsr "main", HALT] <> fundecls_code <> if needOutOfBoundExpectionRuntimeCode then outOfBoundExpectionRuntimeCode else []
             where isVarDecl f = case f of {FunDecl {} -> False; VarDecl {} -> True}
                   isFunDecl f = case f of {FunDecl {} -> True; VarDecl {} -> False}

-- Yo jsr looks to be dynamic function calls!

instance GenSSM (Decl TypecheckedP) where
      generate :: Decl TypecheckedP -> Env Code
      generate  (FunDecl _ name VoidType [] [] body) = do
            setUnlinkNotNeeded
            return [ LABEL name ] <> generate body

      -- Put the var decl on the heap, as in at the start save the heap start into a regsiter. When we then want to load vars we 
      --  Load from that register add the offset that we know and load that. 
      -- This only works if we do the var decls first but we do because we hoist them to the top
      generate (VarDecl _ name _ expr)  = do
            -- These are only global vars!
            globalvarcount <- increaseGlobalVarCount
            modify $ \env@(Info {genEnv = genenv}) -> env {genEnv = Map.insert (Var name) (GlobalVar globalvarcount (getType expr)) genenv}
            generate expr -- The program generator generates the actual saving cause we do it in one go
      generate  (FunDecl _ name _ args funvardecl body) = do
            -- Load the args and funvardecls into the enviroment
            Info {genEnv = original_genenv } <- get
            -- Get the args ;
            let argEnv = Map.fromList [(Var argname, LocalVar i argtype) | ((argname, argtype), i) <- zip args [1..]]
                argcount = length args
                -- So we load from -(argcount + 1) from the mark pointer and we actually load argcount values from there. 
                -- -(argcount + 1) gives -2 with argcount = 1 and -3 with argcount = 2 and so on. Argcount = 0 gives [] so that ok. 
                                                                                  -- Then store these values from the stack as the first argcount locals
                argLoadCode = [LDML (-(argcount + 1)) argcount | argcount > 0] ++ [STML 1 argcount | argcount > 0]
                declEnv = Map.fromList [(Var varname, LocalVar i (getType varexpr)) | (VarDecl _ varname _ varexpr, i) <- zip funvardecl [(argcount+1)..]]
            replaceGenEnv $ original_genenv <> argEnv <> declEnv
            let local_length = length args + length funvardecl
            when (local_length > 0) setUnlinkNeeded
            fundecl_code <- foldMap (\(local_index, decl) -> generate (getExpr decl) <> pure [STL local_index]) $ zip [(1+argcount)..] funvardecl -- Important to store fun var decls as you go otherwise later ones refering to earlier ones get 0 
            -- debugEnv
            body_code <- generate body
            replaceGenEnv original_genenv -- Restore previous gen env
            -- make space for locals, load the fundecls, the arguments should already be there? 
            return $ [LABEL name] <> [LINK local_length | local_length > 0] <> argLoadCode <> fundecl_code {-<> [Annote MP (-1) local_length Pink "Locals" | local_length > 0]-} <> body_code <> [UNLINK | local_length > 0]
            -- todo remove extra return by having phase that explicity adds the implcit return

-- Loading global vars is

loadVar :: String -> Env Code
loadVar name = do
      value <- getkey (Var name)
      case value of
            (GlobalVar offset _) -> return [LDR gvr, LDA offset]
            (LocalVar offset _) -> return [LDL offset]

-- Returns code to store the value on the head of the stack into a variable
storeVar :: String -> Env Code
storeVar name = do
      value <- getkey (Var name)
      case value of
            (GlobalVar offset _) -> return [LDR gvr, STA offset]
            (LocalVar offset _) -> return [STL offset]


instance GenSSM (Literal TypecheckedP) where
      generate TrueLit = pure [LDC (-1)] -- There is also a True and False but its just a bit pattern https://webspace.science.uu.nl/~hage0101/SSM/ssmtopics.html#True
      generate FalseLit = pure [LDC 0]
      generate (IntLit int)  = pure [LDC int]
      generate (CharLit char)  = pure [LDC $ ord char] 
      generate EmptyListLit = pure [LDC 0, LDC 0, STMH 2] -- address of 0 marks the end of the array!
      -- A tuple always has two values. These do not have to be other tuples. In theory the type checker knows what kind of tuple it wants to get.
      -- So lets make it [value1, value2], one of these can be an address but you don't know.
      -- We do have this type information though! In expressions of TupleLit. So we could encode it at the runtime.
      generate (TupleLit (e1, e2)) = generate e1 <> generate e2  <> pure [STMH 2]

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


-- Does not include new lines!
printStringCode :: String -> Code
printStringCode str =  [LDC 0] <> reverse (foldMap (\c -> [LDC (ord c)]) str) <> [TRAP 2]

-- todo We know that this function always has 1 argument so we could do register arg passing, load R6 and then the bool code and then return 

generatePrint :: Type -> Code
generatePrint ty = case Debug.trace (";; Printing the type " ++ show ty) ty of
                              (TypeVar tyname False) -> printStringCode ("Non rigid TypeVar "++ show tyname ++", idk how to print this!\n")
                              (TypeVar tyname True) -> printStringCode ("Rigid TypeVar"++ show tyname ++", idk how to print this!\n")
                              CharType -> [TRAP 1] -- Print adds a newline
                              BoolType -> let trueCode = printStringCode "True"
                                              branchSize = instrSize (BRF undefined)
                                              falseCode = printStringCode "False"
                                            in [LABEL "'printBool", BRF (codeSize trueCode + branchSize)] <> trueCode <> [BRA (codeSize falseCode)] <> falseCode
                              IntType -> [TRAP 0] -- Print does not add a newline, for a newline call print without arguments
                              ListType IntType -> [
                                                  LABEL "'printIntList", LDC (ord '['), TRAP 1,
                                                   LDS 0, -- Remember adress of cons 
                                                   LDA 0, -- Load the adress
                                                   BRF 20,  -- If its empty list skip to the end
                                                   LDS 0, -- Remember the cons cell for the next LDA
                                                   LDA (-1), -- Otherwise load the value 
                                                   TRAP 0,  -- Print it 
                                                   LDA 0, -- Move to the next cons cell 
                                                   LDS 0, -- Check if we should end it or print a comma and continue
                                                   LDA 0,
                                                   BRF 6,
                                                   LDC (ord ','), -- print comma 
                                                   TRAP 1,
                                                   BRA (-20),  -- Jump back to the copy after the inital emtpy check
                                                   LDC (ord ']'), TRAP 1, AJS (-1)]
                              ListType CharType ->  [LABEL "'printChrList",LDS 0, -- Save the adress of the cons cell
                                                     LDA 0, -- Load the first adress
                                                     BRF 10,  -- If its empty list skip to the end
                                                     LDS 0, -- Remember the cons cell for the next LDA
                                                     LDA (-1), -- Load the value 
                                                     TRAP 1,  -- Print it 
                                                     LDA 0, -- Move to the next cons cell 
                                                     BRA (-16), AJS (-1)]   -- Jump back to the brf 12
                              ListType BoolType -> let printBoolCode = generatePrint BoolType
                                                       printBoolCodeSize = codeSize printBoolCode
                                                   in [LABEL "'printBoolList",LDC (ord '['), TRAP 1,
                                                    LDS 0, -- Check for empty list once 
                                                    LDA 0,
                                                    BRF (18 + printBoolCodeSize),
                                                    LDS 0, -- Remember the cons cell for the next LDA
                                                    LDA (-1) -- Otherwise load the value 
                                                    ] <> printBoolCode <> [
                                                    LDA 0, -- Load the next adress
                                                    LDS 0, -- Remember adress 
                                                    LDA 0, -- Load the next cons cell to check for empty 
                                                    BRF 6,  -- If its empty list skip to the end
                                                    LDC (ord ','),
                                                    TRAP 1,
                                                    BRA ((-20) - printBoolCodeSize +2),   -- Jump back to the brf 12
                                                    LDC (ord ']'), TRAP 1, AJS (-1)] -- These prints leave 1 value on the stack always just clean that up
                              -- Detect printing a list of strings to put "" around it
                              ListType (ListType CharType) -> let printItemCode = generatePrint (ListType CharType)
                                                                  printItemCodeSize = codeSize printItemCode
                                                                  in [LABEL "'printStringList",LDC (ord '['), TRAP 1,
                                                                  LDS 0, -- Check for empty list once 
                                                                  LDA 0,
                                                                  BRF (30 + printItemCodeSize), -- Jump over closing "
                                                                  LDC (ord '"'), TRAP 1,
                                                                  LDS 0, -- Remember the cons cell for the next LDA
                                                                  LDA (-1) -- Otherwise load the value 
                                                                  ] <> printItemCode <> [
                                                                  LDA 0, -- Load the next adress
                                                                  LDS 0, -- Remember adress 
                                                                  LDA 0, -- Load the next cons cell to check for empty 
                                                                  BRF 10,  -- If its empty list skip to the end
                                                                  LDC (ord '"'), TRAP 1,
                                                                  LDC (ord ','),
                                                                  TRAP 1,
                                                                  BRA ((-28) - printItemCodeSize +2),   -- Jump back to the brf 12
                                                                  LDC (ord '"'), TRAP 1, LDC (ord ']'), TRAP 1, AJS (-1)] -- These prints leave 1 value on the stack always just clean that up 
                              ListType itemType -> let printItemCode = generatePrint itemType
                                                       printItemCodeSize = codeSize printItemCode
                                                   in [LABEL "'printListList",LDC (ord '['), TRAP 1,
                                                    LDS 0, -- Check for empty list once 
                                                    LDA 0,
                                                    BRF (18 + printItemCodeSize),
                                                    LDS 0, -- Remember the cons cell for the next LDA
                                                    LDA (-1) -- Otherwise load the value 
                                                    ] <> printItemCode <> [
                                                    LDA 0, -- Load the next adress
                                                    LDS 0, -- Remember adress 
                                                    LDA 0, -- Load the next cons cell to check for empty 
                                                    BRF 6,  -- If its empty list skip to the end
                                                    LDC (ord ','),
                                                    TRAP 1,
                                                    BRA ((-20) - printItemCodeSize +2),   -- Jump back to the brf 12
                                                    LDC (ord ']'), TRAP 1, AJS (-1)] -- These prints leave 1 value on the stack always just clean that up
                              -- ListType (ListType a) -> 
                              ty' -> printStringCode ("Error: Can not print value of type " ++ showTypeWithoutColor ty' ++ "\n")

instance GenSSM (Expr TypecheckedP) where
      generate (BinOpExpr _ op left right) = generate left <> generate right <> generate op
      generate (UnaryOpExpr _ op operand) = generate  operand <> generate op
      -- Now it would be really nice if we could know the type of arg 
      generate (FunctionCallExpr _ "exit" _) = pure [HALT]
      generate (FunctionCallExpr _ "print" []) = pure [LDC newline, TRAP 1]
      generate (FunctionCallExpr _ "print" [arg]) = generate arg <> pure (generatePrint (getType arg))

                              -- IntType -> pure [TRAP 0, LDC newline, TRAP 1] -- Print adds a newline

      -- In theory this is it for isEmpty cause last cons cell is 0 addr
      generate (FunctionCallExpr _ "isEmpty" [arg]) =  generate arg <> pure [LDA 0, LDC 0, EQ]
      -- generate (FunctionCallExpr meta "print" (_:_)) = error $ "At " ++ (showStart meta ) + " you call print with too many arguments"
      generate (FunctionCallExpr (ty, _) func args) = do argcode <- generate args                                         -- Last part removes arguments from the stack again!
                                                         let argcount = length args
                                                             removeArgs = [AJS (-argcount) | argcount > 0]
                                                         return $ argcode <> [Bsr func] <> removeArgs <> [LDR RR | ty /= VoidType]
      generate (VariableExpr _ (Identifier varname Nothing)) = loadVar varname
      generate (VariableExpr (_, meta) (Identifier varname (Just HeadField))) = includeOutOfBoundsRuntimeExceptionCode >> loadVar varname <> pure ( checkBounds <> [LDA (-1)])
            where checkBounds1 = [LDS 0, LDA 0]
                  checkBounds2 = [LDC (startCol meta), LDC (startLine meta), Bra "'outOfBoundExpection"]
                  jumpSize = codeSize checkBounds2
                  checkBounds = checkBounds1 <> [BRT jumpSize] <> checkBounds2
      generate (VariableExpr _ (Identifier varname (Just TailField))) = loadVar varname  <> pure [LDA 0]
      generate (VariableExpr _ (Identifier varname (Just FirstField))) = loadVar varname <> pure [LDA (-1)]
      generate (VariableExpr _ (Identifier varname (Just SecondField))) = loadVar varname <> pure [LDA 0]
      generate (LiteralExpr _ literal) = generate literal


-- We can generate the enviroment before we do any code generation because we know where it will end up
-- buildFunEnv :: [Decl TypecheckedP] -> Env Code -> Env Code
-- buildFunEnv decls env = env <> Map.fromList (zip (map nameToVarKey decls) ([LocalVar i | i <- [1..]] <*> map getDeclType decls))
      -- where nameToVarKey (VarDecl _ name _ _) = Var name
            -- nameToVarKey (FunDecl _ name _ _ _ _) = Fun name

-- Ok laten we eerst maar gewoon even 5 keer een nop of halt genereren ofzo

getExpr :: Decl TypecheckedP -> Expr TypecheckedP
getExpr (VarDecl _ _ _ expr)  = expr
getExpr f@(FunDecl {}) = error $ "Can only get expressions from vardecl not fun delc" ++ show f


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
      generate Mul = pure [MUL]
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
{- For (:) (cons), the stack looks like this:

|  value  |
|   addr  |
|  .....  |

The application of the cons operation should
put (value, addr) on the heap, and put the
address back on the stack at the top.

We can not change that value is first and then adress becausea cons bin op goes left to right and left is the value and right the adress. So (value, adress)
Because we do stmh the adress which points to the adress in the heap of the previous cell is stored.

-}
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