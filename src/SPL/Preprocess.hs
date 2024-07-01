{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module SPL.Preprocess (preprocesAST, checkHasMain, checkEmptyBody) where



import Data.List (sortBy)
import SPL.AST

import qualified Debug.Trace as Debug
import SPL.Parser.SourceSpan
import SPL.Colors (red, blue, yellow)
import SPL.Return
import SPL.ArgParse (Args, hideWarnings)

compareDecl :: Decl p1 -> Decl p2 -> Ordering
compareDecl (VarDecl {}) (FunDecl {}) = LT
compareDecl (FunDecl {}) (VarDecl {}) = GT
compareDecl (FunDecl {}) (FunDecl {}) = EQ
compareDecl (VarDecl {}) (VarDecl {}) = EQ

{- Sorts the program, putting the global vars at start -}
hoistGlobalVars :: Program ParsedP -> Program ParsedP
hoistGlobalVars = sortBy compareDecl

{- Remove all the code behind a return statement -}
removeDeadCode :: Args -> Program ParsedP -> Program ParsedP
removeDeadCode _ [] = []
removeDeadCode programArgs (var@VarDecl {}:later) = var : removeDeadCode programArgs later
removeDeadCode programArgs (FunDecl meta name ty args funvars body:later) =  FunDecl meta name ty args funvars (removeStmtAfterReturns programArgs body): removeDeadCode programArgs later

warn :: Args -> String -> a -> a
warn args message value = if not (hideWarnings args) then Debug.trace message value else value

removeStmtAfterReturns :: Args -> [Stmt ParsedP] -> [Stmt ParsedP]
removeStmtAfterReturns _ [] = []
removeStmtAfterReturns programArgs (ret@(ReturnStmt meta _): rest) = if not $ null rest then warn programArgs (yellow "WARNING: " ++ "Removing dead code after return at " ++ showEnd meta) [ret] else [ret]
removeStmtAfterReturns programArgs (iff@(IfStmt meta _ consequent  (Just alternative)): rest) = case (returns consequent, returns alternative) of
                                                                                         -- The lefts are already handeld at this point! but doesn't hurt to be save
                                                                                         (Left err, _) -> error err
                                                                                         (_, Left err) -> error err
                                                                                         (Right (WithValue _), Right (WithValue _)) -> if null rest then [iff] else warn programArgs (yellow "WARNING: "++"Removing dead code after if at " ++ showEnd meta) [iff]
                                                                                         (Right (WithoutValue _), Right (WithoutValue _)) -> if null rest then [iff] else warn programArgs (yellow "WARNING: " ++ "Removing dead code after if at " ++ showEnd meta) [iff]
                                                                                         _ -> iff : removeStmtAfterReturns programArgs rest
removeStmtAfterReturns programArgs (a: later) = a : removeStmtAfterReturns programArgs later

preprocesAST :: Args -> Program ParsedP -> Program ParsedP
preprocesAST args =  hoistGlobalVars . removeDeadCode args

checkHasMain :: Program ParsedP -> Either String ()
checkHasMain [] = Left (red "No main function in your program! " ++ "\nPlease add a main function to your program.")
checkHasMain (FunDecl _ "main" _ (_:_) _ _:_) = Left (red "The '" ++ blue "main" ++ red "' function can not have any arguments."++"\nThere is no way for you to initialize them. Please remove the arguments.")
checkHasMain (FunDecl _ "main" _ _ _ _:_) = Right ()
checkHasMain (_:later) = checkHasMain later

checkEmptyBody :: Program ParsedP -> Either String ()
checkEmptyBody [] = Right ()
checkEmptyBody (FunDecl meta name _ _ _ [] :_) = Left (red "The '" ++ blue name ++ red "' function declared at "++ showStart meta ++ red " has an empty body."++"\nEmpty functions are not allowed. Please either add a body (just a '" ++ red "return" ++ ";' is enough) or remove the function.")
checkEmptyBody (_:later) = checkEmptyBody later