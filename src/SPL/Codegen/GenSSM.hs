{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SPL.Codegen.GenSSM where

import SPL.AST

import SPL.Codegen.SSM
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
-- import Control.Monad.Reader
import Control.Monad.State.Lazy
import SPL.Parser.SourceSpan
import SPL.Colors (blue, red, black, yellow, bold)
import Prelude hiding (EQ)
import SPL.ArgParse (Args (hideWarnings, skipOptimizer), hideWarnings)

data Location =  LocalVar  Int Type | GlobalVar Int Type
              deriving (Eq, Ord, Show)
data Key = Fun String | Var String
              deriving (Eq, Ord, Show)

-- Map from a variable name to the code required to get the value on the stack
data Info = Info {
                   genEnv :: Map.Map Key Location, -- genEnv has the data needed to generate code like variables and where they are 
                   globalVarCounter :: Int,
                   needsUnlink :: Bool,
                   needsOutOfBoundsRuntimeExeptionCode :: Bool,
                   programArgs :: Args
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
includeOutOfBoundsRuntimeExceptionCode = warn "Including list out of bounds runtime" >> modify (\env -> env {needsOutOfBoundsRuntimeExeptionCode = True})

replaceGenEnv :: Map.Map Key Location -> Env ()
replaceGenEnv genenv = modify (\env -> env {genEnv = genenv})


logEnvBlack :: Applicative f => [Char] -> f ()
logEnvBlack s = Debug.trace (black s) pure ()
logEnv :: Applicative f => String -> f ()
logEnv s = Debug.trace s pure ()

debugEnv :: Env ()
debugEnv = do
      (Info env counter needUnlink needsOutOfBoundsRuntimeCode _) <- get
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

-- Generate INLINE code that prints a runtime error and then halts. Only for simple exceptions.
-- For exceptions with arguments include something in the runtime and pop of the stack 
throw :: String -> Code
-- throw msg = [LDS i | i <- chars] <> [TRAP 1 | _ <- chars] <> [HALT] where chars = map ord msg 
throw msg = [LDS 0] <> reverse [LDS i | i <- chars] <> [TRAP 2] <> [HALT] where chars = map ord msg

-- To use this runtime function, first push the column and then the line number. Do not push return adress on the stack as there is no returning from this
outOfBoundExpectionRuntimeCode :: Code
outOfBoundExpectionRuntimeCode = [LABEL "'outOfBoundExpection"] <> printStringCode "\nArray out of bounds runtime expection:\nTrying to access .hd on an empty array. \nCaused by line "
                                 <> [TRAP 0] <> printStringCode " column " <> [TRAP 0, LDC (ord '.'), TRAP 1, HALT]

emptyEnv :: Args -> Info
emptyEnv = Info Map.empty (-1) False False


getSmmCode :: Args -> Program TypecheckedP -> Code
getSmmCode args p = evalState (generate p) $ emptyEnv args

warn :: String -> Env ()
warn msg = do
      args <- gets programArgs
      unless (hideWarnings args) $ Debug.trace (yellow "WARNING: " ++ msg) pure ()

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
      generate  (FunDecl _ name VoidType [] [] body) = pure [ LABEL name ] <> generateLINK 0 <> generate body <* setUnlinkNotNeeded
      -- Put the var decl on the heap, as in at the start save the heap start into a regsiter. When we then want to load vars we load from that register add the offset that we know and load that. 
      -- This only works if we do the var decls first but we do because we hoist them to the top
      generate (VarDecl _ name _ expr)  = do -- These are only global vars! Fun var decls are generated when we generate the the code for the fun decl
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
            linkCode <- generateLINK local_length -- This should happen first so that it knows if it has to unlink
            fundecl_code <- foldMap (\(local_index, decl) -> generate (getExpr decl) <> pure [STL local_index]) $ zip [(1+argcount)..] funvardecl -- Important to store fun var decls as you go otherwise later ones refering to earlier ones get 0 
            -- debugEnv
            body_code <- generate body
            replaceGenEnv original_genenv -- Restore previous gen env
            -- make space for locals, load the fundecls, the arguments should already be there? 
            ([LABEL name] <> linkCode <> argLoadCode <> fundecl_code <> body_code) <$ setUnlinkNotNeeded -- Unlink is always added because we always add the return statement
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
      generate (AssignStmt _ (Identifier name (Just HeadField)) expr) = generate expr <> loadVar name <> pure [STA (-1)] -- Woah who would have thought that the field assignments is the same code 
      generate (AssignStmt _ (Identifier name (Just TailField)) expr) = generate expr <> loadVar name <> pure [STA 0]
      generate (AssignStmt _ (Identifier name (Just FirstField)) expr) = generate expr <> loadVar name <> pure [STA (-1)]
      generate (AssignStmt _ (Identifier name (Just SecondField)) expr) = generate expr <> loadVar name <> pure [STA 0]
      generate (ReturnStmt _ (Just e)) = generate e <> pure [STR RR] <> generateUNLINK <> pure [RET]
      generate (ReturnStmt _ Nothing) = generateUNLINK <> pure [RET]
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
            bodyCode <- generate body
            let branchLength = instrSize (BRA undefined)
                bodyLenght = codeSize bodyCode + branchLength
                conditionLength = codeSize conditionCode + branchLength
                bodyCodeWithBranch = bodyCode ++ [BRA (-(bodyLenght + conditionLength))] -- jump back up to condition
                conditionCodeWithBranch = conditionCode ++ [BRF bodyLenght] -- skip body on false
            return $ conditionCodeWithBranch <> bodyCodeWithBranch
      generate (BlockStmt list) = foldMap generate list


-- Generates an unlink if needed
-- If we skip optimizer we should always unlink because if we don't optimise we might leave expressions on the stack and then we return to result of expression ...
generateUNLINK :: Env Code
generateUNLINK = do
      args <- gets programArgs
      unlinkNeeded <- gets needsUnlink
      let shouldUnlink = unlinkNeeded || skipOptimizer args
      return [UNLINK | shouldUnlink]

generateLINK :: Int -> Env Code
generateLINK numberOfLocals = do
      args <- gets programArgs
      let shouldLink = numberOfLocals > 0 || skipOptimizer args
      when shouldLink setUnlinkNeeded
      return [LINK numberOfLocals | shouldLink]

-- we could reverses the code 

newline :: Int
newline = 10


-- Does not include new lines!
printStringCode :: String -> Code
printStringCode str =  [LDC 0] <> reverse (foldMap (\c -> [LDC (ord c)]) str) <> [TRAP 2]

-- todo We know that this function always has 1 argument so we could do register arg passing, load R6 and then the bool code and then return 

generatePrint :: Type -> Code
generatePrint ty = case {- Debug.trace (";; Printing the type " ++ show ty)-} ty of
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
                              TupleType a@(ListType CharType) b@(ListType CharType) -> 
                                                 let printAcode = generatePrint a
                                                     printBcode = generatePrint b
                                                 in [
                                                      LABEL "'printTupleStr12", LDC (ord '('), TRAP 1,
                                                      LDS 0,  -- Copy for the second value
                                                      LDA (-1)
                                                 ] <> [LDC (ord '"'), TRAP 1] <> printAcode <> [LDC (ord '"'), TRAP 1] <>
                                                      [LDC (ord ','), TRAP 1, LDC (ord ' '), TRAP 1, LDA 0] -- Print `, ` and load next value
                                                    <> [LDC (ord '"'), TRAP 1] <> printBcode  <> [LDC (ord '"'), TRAP 1] <> [LDC (ord ')'), TRAP 1]
                              TupleType a b@(ListType CharType) -> 
                                                 let printAcode = generatePrint a
                                                     printBcode = generatePrint b
                                                 in [
                                                      LABEL "'printTupleStr2", LDC (ord '('), TRAP 1,
                                                      LDS 0,  -- Copy for the second value
                                                      LDA (-1)
                                                 ] <> printAcode <>
                                                 [LDC (ord ','), TRAP 1, LDC (ord ' '), TRAP 1, LDA 0] -- Print `, ` and load next value
                                                 <> [LDC (ord '"'), TRAP 1] <> printBcode <> [LDC (ord '"'), TRAP 1] <> [LDC (ord ')'), TRAP 1]
                              TupleType a@(ListType CharType) b -> 
                                                 let printAcode = generatePrint a
                                                     printBcode = generatePrint b
                                                 in [
                                                      LABEL "'printTupleStr1", LDC (ord '('), TRAP 1,
                                                      LDS 0,  -- Copy for the second value
                                                      LDA (-1)
                                                 ] <> [LDC (ord '"'), TRAP 1] <> printAcode <> [LDC (ord '"'), TRAP 1] <>
                                                      [LDC (ord ','), TRAP 1, LDC (ord ' '), TRAP 1, LDA 0] -- Print `, ` and load next value
                                                   <> printBcode <> [LDC (ord ')'), TRAP 1]
                              
                              TupleType a b  -> let printAcode = generatePrint a
                                                    printBcode = generatePrint b
                                                 in [
                                                      LABEL "'printTuple", LDC (ord '('), TRAP 1,
                                                      LDS 0,  -- Copy for the second value
                                                      LDA (-1)
                                                 ] <> printAcode <>
                                                 [LDC (ord ','), TRAP 1, LDC (ord ' '), TRAP 1, LDA 0] -- Print `, ` and load next value
                                                 <> printBcode <> [LDC (ord ')'), TRAP 1]
                              ty' -> printStringCode ("Error: Can not print value of type " ++ showTypeWithoutColor ty' ++ "\n")


-- Function used to check if we can compare two types, if this is false the compare code should result in [LDC 0]
-- Comparing two type vars of different names is false but shall give a warning.
compareCode :: SourceSpan -> Type -> Type -> Env Code
compareCode _ CharType CharType = generate Eq
compareCode _ IntType IntType = generate Eq
compareCode _ BoolType BoolType = generate Eq
-- We know we can compare them savely because the typechecker checked it? What does the typechecker check?
compareCode meta (TupleType t1a t1b) (TupleType t2a t2b) = do 
                                                            aEqCode <- compareCode meta t1a t2a
                                                            bEqCode <- compareCode meta t1b t2b
                                                            let bEqCodeLen = codeSize bEqCode  
                                                            return $ [LDS 0, LDA (-1),-- Load the left of tuple 1
                                                                      LDS (-2), LDA (-1) -- Load the left of tuple 2
                                                                     ] <> aEqCode <> -- Leaves 1 single True or False
                                                                  -- If not equal we jump to the end and push false, else you have to compare the other item
                                                                     [BRT 4, LDC 0, BRA (8 + bEqCodeLen)] <>
                                                                     [LDS 0, LDA 0, -- Load right of tuple 1
                                                                      LDS (-2), LDA 0]  -- Load left of tuple 2
                                                                     <> bEqCode -- The result of this equal is the final result
                                                                     <> [STS (-2), AJS (-1)] -- Remove used stack space

compareCode meta (ListType t1) (ListType t2) = do
       comparecode <- compareCode meta t1 t2
       let cmpSize = codeSize comparecode
       pure [LDS 0, LDA 0, LDC 0] <> generate Neq <>  -- check if list 1 is at ts the end (but with Neq because then we can do and later)
            pure [LDS (-2), LDA 0, LDC 0] <> generate Neq  <> -- check if list 2 is at the end 
            pure [LDS 0, LDS (-2), XOR, BRF 6, AJS (-4), LDC 0, BRA (17 + cmpSize + 18)] <> -- If the xor is true (1,0 or 0,1) the whole result is false because not the same length, how do we clean up????
            pure [AND {- We already know its the same because of the xor, so this either gets (1,1) or (0,0). So if the And is false we know there is no next value, If the and is true then the next value adresses are both NOT zero so there is a next value, we know there is a next value-}, 
                      BRT 6, AJS (-2), LDC (-1), BRA (8 + cmpSize + 10 + 8)] <>  -- Check if they are both 0 because if thats the case everything is true since we got here
            pure [LDS 0, LDA (-1), LDS (-2), LDA (-1)]  <>    -- Load the values, using a copy of the adress so we can load the next adress next time
            pure comparecode <>  -- compare the values,  
            pure [BRT 6, AJS (-2), LDC 0,  BRA 10] <> -- If its false jump to the end 
            pure [LDA 0, AJS (-1), LDA 0, AJS 1] <> -- Move forward in both lists
            pure [BRA ((-10) - cmpSize - 52)] -- Otherwise jump back to the top which will actually consume the values?

       -- If these are both at the end we now have T T on the stack, if they are both not empty we have False False on the stack
            -- If the xor is true the result of the whole comparison is false
            -- If both are true the result of the whole comparison is true
            -- If both are false we need to actually check the list argument 
            -- If one is true and other fales everything is false 
            
compareCode meta t1@(TypeVar name True) t2@(TypeVar name' True) =
      if name == name'  then warn ("You are comparing two rigid type variables: " ++ show t1 ++ " and " ++ show t2 ++
                        " at " ++ showStart meta ++ ".\nThese two rigid type variables share the same name, " ++
                        "which means they will have the same type. However, this comparison will be a simple reference " ++
                        "comparison, not a value comparison. \n\nExamples of reference comparisons:\n" ++
                        "- 1 == 1 -> True\n" ++
                        "- 1 == 2 -> False\n" ++
                        "- \"Hi\" == \"Hi\" -> False (These strings have the same value but different memory locations)\n" ++
                        "- a == a -> True (Always true, since it's comparing the same variables and thus the same memory location)"++
                        "\n\nIn summary, this comparison will work for primitive types like Int or Bool, "++
                        "but not for complex types like Lists, Tuples, or Strings"++
                        " unless they refer to the same memory location. \nEnsure this behavior is intended for your code.")
                        >> generate Eq
                       else warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++
                                  ". These two rigid typevars have different names which means the values should have different types."++
                                  "For this reason the comparison shall always result in False! "++
                                  "\nEven if during the runtime they will be instanciated as the same type the equality will ALWASY be False!. "++
                                  "\nEnsure this behavior is what you want.")
                             >> pure [LDC 0] -- warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++". These two typevars have different names and as such this comparison will always be False! Even if during the runtime they will be instanciated as the same type the equality will ALWAYS be False!. Make sure this is what you want.") >> generate Eq
compareCode meta t1@(TypeVar name False) t2@(TypeVar name' False) =
      if name == name'  then warn ("You are comparing two type variables: " ++ show t1 ++ " and " ++ show t2 ++
                        " at " ++ showStart meta ++ ".\nThese two type variables share the same name, " ++
                        "which means they will have the same type. However, this comparison will be a simple reference " ++
                        "comparison, not a value comparison. \n\nExamples of reference comparisons:\n" ++
                        "- 1 == 1 -> True\n" ++
                        "- 1 == 2 -> False\n" ++
                        "- \"Hi\" == \"Hi\" -> False (These strings have the same value but different memory locations)\n" ++
                        "- a == a -> True (Always true, since it's comparing the same variables and thus the same memory location)"++
                        "\n\nIn summary, this comparison will work for primitive types like Int or Bool, "++
                        "but not for complex types like Lists, Tuples, or Strings"++
                        " unless they refer to the same memory location. \nEnsure this behavior is intended for your code.")
                        >> generate Eq
                       else warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++
                                  ". These two typevars have different names which means the values can have different types."++
                                  "For this reason the comparison shall always result in False! "++
                                  "\nEven if during the runtime they will be instanciated as the same type the equality will ALWASY be False!. "++
                                  "Ensure this behavior is what you want.")
                             >> pure [LDC 0]
compareCode meta t1@(TypeVar _ False) t2@(TypeVar _ True) =
      warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++
            ".\nThe first typevar is rigid which means that you (the programmer) specified that the value could be any type and that "++
            "the type should never be narrowed.\nThe second typevars is infered by me (the compiler) and thus is non rigid."++
            "This means that I decided that the value can be any type."++
            "\n\nI can not generate code that can properly compare a rigid type var with a non rigid typevar. "++
            "This means that during runtime, if you compare a rigid typevar with a non rigid typevar the result will "++ bold "always"++" be False. "++
            " Make sure this is what you want.")
      >> pure [LDC 0]
compareCode meta t1@(TypeVar _ True) t2@(TypeVar _ False) =
      warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++
            ".\nThe first typevar is infered by me (the compiler) and thus is non rigid." ++
            "This means that I decided that the value can be any type.\nThe second typevar is rigid " ++
            "which means that you (the programmer) specified that the value could be any type and that " ++
            "the type should not be narrowed." ++
            "\n\nI can not generate code that can properly compare a rigid type var with a non rigid typevar. " ++
            "This means that during runtime, if you compare a rigid typevar with a non rigid typevar the result will " ++ bold "always" ++ " be False. " ++
            " Make sure this is what you want.")
      >> pure [LDC 0]
compareCode _ (FunType {}) (FunType {}) = generate Eq -- Even though we don't have function types. For this one lets actually compare address
compareCode meta VoidType VoidType = error $ red "You can not compare two void types." ++ "But you tried to do it here: " ++ showStart meta
compareCode meta t1 t2 = warn ("You are comparing a " ++ show t1 ++ " with a "++ show t2 ++" at "++ showStart meta ++". Because these are different types the result will ALWAYS be False. Make sure this is what you want.")
                         >> pure [LDC 0]



-- Get the types 
-- If not the same its LDC 0
-- If the same compare normally except container types.
-- First do container with primitive types.



instance GenSSM (Expr TypecheckedP) where
      -- Overloaded EQ
      generate (BinOpExpr (_,meta) Eq left right) = generate left <> generate right <> compareCode meta (getType left) (getType right)
      generate (BinOpExpr (_,meta) Neq left right) = generate left <> generate right <> compareCode meta (getType left) (getType right) <> generate Neq 
      generate (BinOpExpr _ op left right) = generate left <> generate right <> generate op
      -- we need to do unaryopexpr here cause we need the meta. I tried adding meta to field access in the metafield branch but it didn't work with nested field access
      generate (UnaryOpExpr _ Negate operand) = generate  operand <> pure [NOT]
      generate (UnaryOpExpr _ Min operand) = generate operand <> pure [NEG]
      generate (UnaryOpExpr (_,meta) (FieldAccess HeadField) operand) = generate  operand <> headaccess meta
      generate (UnaryOpExpr _ (FieldAccess TailField) operand) = generate  operand <> pure tailaccess
      generate (UnaryOpExpr _ (FieldAccess FirstField) operand) = generate  operand <> pure [LDA (-1)]
      generate (UnaryOpExpr _ (FieldAccess SecondField) operand) = generate  operand <> pure [LDA 0]
      -- Now it would be really nice if we could know the type of arg 
      generate (FunctionCallExpr _ "printIntAsChar" [arg]) = generate arg <> pure [TRAP 1]
      generate (FunctionCallExpr _ "getChar" _ ) = pure [TRAP 11]
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
      generate (VariableExpr (_, meta) (Identifier varname (Just HeadField))) = loadVar varname <> headaccess meta
      generate (VariableExpr _ (Identifier varname (Just TailField))) = loadVar varname  <> pure tailaccess
      generate (VariableExpr _ (Identifier varname (Just FirstField))) = loadVar varname <> pure [LDA (-1)]
      generate (VariableExpr _ (Identifier varname (Just SecondField))) = loadVar varname <> pure [LDA 0]
      generate (LiteralExpr (_, meta) literal@(IntLit value)) = do
            when (value > 2147483646) (warn ( "Int literal is too large (" ++ show literal ++ " > " ++ black "2147483647"++"). It probably won't work properly in ssm. At " ++ showStart meta))
            when (value < -2147483647) (warn ( "Int literal is too small (" ++ show literal ++ " < " ++ black "-2147483647"++"). It probably won't work properly in ssm. At " ++ showStart meta))
            generate literal
      generate (LiteralExpr _ literal) = generate literal


headaccess :: SourceSpan -> Env Code
headaccess meta = includeOutOfBoundsRuntimeExceptionCode >> pure ( checkBounds <> [LDA (-1)])
            where checkBounds1 = [LDS 0, LDA 0]
                  checkBounds2 = [LDC (startCol meta), LDC (startLine meta), Bra "'outOfBoundExpection"]
                  jumpSize = codeSize checkBounds2
                  checkBounds = checkBounds1 <> [BRT jumpSize] <> checkBounds2

tailaccess :: Code
tailaccess = [LDS 0, LDA 0,  -- Check zero
              BRF 2, -- If its 0 leave the adress unchanged
              LDA 0  -- Else load the tail 
             ]

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