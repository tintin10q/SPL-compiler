{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module SPL.Preprocess (preprocesAST, checkHasMain) where



import Data.List (sortBy)
import SPL.AST

import qualified Debug.Trace as Debug
import SPL.Parser.SourceSpan 
import SPL.Colors (black, red)
import SPL.Return

compareDecl :: Decl p1 -> Decl p2 -> Ordering
compareDecl (VarDecl {}) (FunDecl {}) = LT
compareDecl (FunDecl {}) (VarDecl {}) = GT
compareDecl (FunDecl {}) (FunDecl {}) = EQ
compareDecl (VarDecl {}) (VarDecl {}) = EQ

{- Sorts the program, putting the global vars at start -}
hoistGlobalVars :: Program ParsedP -> Program ParsedP
hoistGlobalVars = sortBy compareDecl

{- Remove all the code behind a return statement -}
removeDeadCode :: Program ParsedP -> Program ParsedP
removeDeadCode [] = []
removeDeadCode (var@VarDecl {}:later) = var : removeDeadCode later
removeDeadCode (FunDecl meta name ty args funvars body:later) =  FunDecl meta name ty args funvars (removeStmtAfterReturns body): removeDeadCode later

removeStmtAfterReturns :: [Stmt ParsedP] -> [Stmt ParsedP]
removeStmtAfterReturns [] = []
removeStmtAfterReturns (ret@(ReturnStmt meta _): rest) = if not $ null rest then Debug.trace (black "WARNING: Removing dead code after return at " ++ showEnd meta) [ret] else [ret]
removeStmtAfterReturns (iff@(IfStmt meta _ consequent  (Just alternative)): rest) = case (returns consequent, returns alternative) of
                                                                                         -- The lefts are already handeld at this point! but doesn't hurt to be save
                                                                                         (Left err, _) -> error err
                                                                                         (_, Left err) -> error err 
                                                                                         (Right (WithValue _), Right (WithValue _)) -> if null rest then [iff] else Debug.trace (black "WARNING: Removing dead code after if at " ++ showEnd meta) [iff]
                                                                                         (Right (WithoutValue _), Right (WithoutValue _)) -> if null rest then [iff] else Debug.trace (black "WARNING: Removing dead code after if at " ++ showEnd meta) [iff]
                                                                                         _ -> iff : removeStmtAfterReturns rest 
removeStmtAfterReturns (a: later) = a : removeStmtAfterReturns later

preprocesAST :: Program ParsedP -> Program ParsedP
preprocesAST = hoistGlobalVars . removeDeadCode

checkHasMain :: Program ParsedP -> Either String ()
checkHasMain [] = Left (red "No main function in your program! " ++ "\nPlease add a main function to your program.")
checkHasMain (FunDecl _ "main" _ _ _ _:_) = Right ()
checkHasMain (_:later) = checkHasMain later
