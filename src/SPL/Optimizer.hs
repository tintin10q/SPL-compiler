{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module SPL.Optimizer where
import qualified Debug.Trace as Debug

import SPL.AST

{-
    We can do certian optimalizations:
    If we have dependend types for booleans we can go even more crazy here
    Make an optimize expression function and then do it on statements 
    Kind of want to fold it on the tree and return the tree again

    Statements 
    If an if statement is false we can remove it 
    If aw whil
-}

class Optimise a where
    opti :: a -> a

-- You kind of want to have a var env here so you can look up the types of the variables
-- It would be nice if we could have bool type and truetype and false type for when we know that variables are true or false
-- It would never work because you can reassign them 
instance Optimise (Expr p) where
    opti lit@(LiteralExpr _ _) = lit
    opti var@(VariableExpr _ _) = var
    opti (BinOpExpr meta And e1 e2) = case (opti e1, opti e2) of
                                        (lit@(LiteralExpr _ TrueLit), _) -> lit
                                        (_, lit@(LiteralExpr _ TrueLit)) -> lit
                                        (lit@(LiteralExpr _ FalseLit), _) -> lit
                                        (_, lit@(LiteralExpr _ FalseLit)) -> lit
                                        (opti_e1, opti_e2) -> BinOpExpr meta And opti_e1 opti_e2
    opti (BinOpExpr meta Or e1 e2) = case (opti e1, opti e2) of
                                        (lit@(LiteralExpr _ TrueLit), _) -> lit
                                        (_, lit@(LiteralExpr _ TrueLit)) -> lit
                                        (LiteralExpr _ FalseLit, other) -> other
                                        (other, LiteralExpr _ FalseLit) -> other
                                        (opti_e1, opti_e2) -> BinOpExpr meta Or opti_e1 opti_e2
    opti (BinOpExpr m op ex1 ex2) = BinOpExpr m op (opti ex1) (opti ex2)
    opti (UnaryOpExpr meta Negate a) = case opti a of
                                            (LiteralExpr m TrueLit) -> LiteralExpr m FalseLit
                                            (LiteralExpr m FalseLit) -> LiteralExpr m TrueLit
                                            expr -> UnaryOpExpr meta Negate expr
    opti unary@(UnaryOpExpr _ (FieldAccess _) _) = unary

    opti (FunctionCallExpr meta funname args) = FunctionCallExpr meta funname (map opti args)

instance Optimise (Stmt p) where
    opti (ExprStmt meta expr) = ExprStmt meta (opti expr)
    opti ass@(AssignStmt {}) = ass
    opti (ReturnStmt meta expr) = ReturnStmt meta (opti <$> expr)
    opti (IfStmt meta condition consequent alternative) = case opti condition of
                                                            (LiteralExpr _ TrueLit) -> BlockStmt (map opti consequent)
                                                            (LiteralExpr _ FalseLit) -> BlockStmt $ maybe [] (map opti) alternative
                                                            cond -> IfStmt meta cond (map opti consequent) (map opti <$> alternative)
                                                            -- IfStmt meta (opti condition) (map empty consequent) (map empty <$> alternative)
    opti (WhileStmt meta condition body) = case opti condition of
                                                true@(LiteralExpr _ TrueLit) -> WhileStmt meta true (map opti body)
                                                LiteralExpr _ FalseLit -> BlockStmt []
                                                expr -> WhileStmt meta expr (map opti body)
    opti (BlockStmt s) = BlockStmt (map opti s)

instance Optimise (Decl p) where
    opti (FunDecl meta funname retty args funvars body) = FunDecl meta funname retty args (map opti funvars) (map opti body)
    opti (VarDecl meta varname ty expr) = VarDecl meta varname ty (opti expr)


collapseBlocks :: Program p -> Program p
collapseBlocks [] = []
collapseBlocks (var@(VarDecl{}):later) = var : collapseBlocks later
collapseBlocks (FunDecl a b c d e body:later) = FunDecl a b c d e (collapseBlocksStmt body) : collapseBlocks later

collapseBlocksStmt :: [Stmt p] -> [Stmt p]
collapseBlocksStmt [] = []
collapseBlocksStmt (IfStmt meta condition consequent alternative:later) = IfStmt meta condition (collapseBlocksStmt consequent) (collapseBlocksStmt <$> alternative):collapseBlocksStmt later
collapseBlocksStmt (WhileStmt meta condition body:later) = WhileStmt meta condition (collapseBlocksStmt body) : collapseBlocksStmt later
collapseBlocksStmt (BlockStmt code:later) = code ++ collapseBlocksStmt later
collapseBlocksStmt (s:later) = s:collapseBlocksStmt later

opti_improvement  :: Show a =>  [a] -> [a] -> Int
opti_improvement p1 p2 = let length_p1 = length $ show p1
                             length_p2 = length $ show p2
                             diff = length_p2 - length_p1
                             in diff * 100 `div` length_p1    