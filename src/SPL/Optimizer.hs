{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.Optimizer where

import SPL.AST
import SPL.Colors (red)
import SPL.Parser.SourceSpan (showStart)

{-
    We can do certian optimalizations:
    If we have dependend types for booleans we can go even more crazy here
    Make an optimize expression function and then do it on statements
    Kind of want to fold it on the tree and return the tree again

    Statements
    If an if statement is false we can remove it
-}

class Optimise a where
  opti :: a -> a

-- You kind of want to have a var env here so you can look up the types of the variables
-- It would be nice if we could have bool type and truetype and false type for when we know that variables are true or false
-- It would never work because you can reassign them
instance Optimise (Expr TypecheckedP) where
  opti lit@(LiteralExpr _ _) = lit
  opti var@(VariableExpr _ _) = var
  -- Boolean Literal evaluation, todo this makes a small part lazy, which means that if you would have 1 && true it would be ok. So its important that this runs after type checking I think
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
  -- Int Literal evaluation
  opti (BinOpExpr meta Mul e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ IntLit $ a * b
                                        (e1', e2') -> BinOpExpr meta Mul e1' e2'
  opti (BinOpExpr meta Div e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr (_,m) (IntLit 0)),  (LiteralExpr _ (IntLit _))) -> error $ red "Detected divide by zero error during optimzing at " ++ showStart m
                                        ((LiteralExpr (_,m) (IntLit _)),  (LiteralExpr _ (IntLit 0))) -> error $ red "Detected divide by zero error during optimzing at " ++ showStart m 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ IntLit $ a `div` b
                                        (e1', e2') -> BinOpExpr meta Div e1' e2'
  opti (BinOpExpr meta Mod e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ IntLit $ a `mod` b
                                        (e1', e2') -> BinOpExpr meta Mod e1' e2'
  opti (BinOpExpr meta Add e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ IntLit $ a + b
                                        (e1', e2') -> BinOpExpr meta Add e1' e2'
  opti (BinOpExpr meta Sub e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ IntLit $ a - b
                                        (e1', e2') -> BinOpExpr meta Sub e1' e2'
  opti (BinOpExpr meta Gt e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a > b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),  (LiteralExpr _ (CharLit b))) -> LiteralExpr m $ if a > b then TrueLit else FalseLit
                                        (e1', e2') -> BinOpExpr meta Gt e1' e2'
  opti (BinOpExpr meta Eq e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)), (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a == b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),(LiteralExpr _ (CharLit b)))-> LiteralExpr m $ if a == b then TrueLit else FalseLit

                                        ((LiteralExpr m TrueLit),  (LiteralExpr _ TrueLit))  -> LiteralExpr m TrueLit
                                        ((LiteralExpr m FalseLit), (LiteralExpr _ FalseLit)) -> LiteralExpr m TrueLit
                                        ((LiteralExpr m TrueLit),  (LiteralExpr _ FalseLit)) -> LiteralExpr m FalseLit
                                        ((LiteralExpr m FalseLit), (LiteralExpr _ TrueLit))  -> LiteralExpr m FalseLit
                                        
                                        ((LiteralExpr m (TupleLit (a,b))), (LiteralExpr _ (TupleLit (c,d))))  -> LiteralExpr m $ if a == c && b == d then TrueLit else FalseLit
                                        ((LiteralExpr m (EmptyListLit)), (LiteralExpr _ EmptyListLit))  -> LiteralExpr m TrueLit -- The code gen also does this even for empty lists of type vars

                                        ((VariableExpr _ name),  (VariableExpr _ name')) -> if name == name' then LiteralExpr meta TrueLit else BinOpExpr meta Eq e1 e2
                                        (e1', e2') -> BinOpExpr meta Eq e1' e2'
  opti (BinOpExpr meta Lt e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a < b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),  (LiteralExpr _ (CharLit b))) -> LiteralExpr m $ if a < b then TrueLit else FalseLit
                                        (e1', e2') -> BinOpExpr meta Lt e1' e2'
  opti (BinOpExpr meta Gte e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a >= b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),  (LiteralExpr _ (CharLit b))) -> LiteralExpr m $ if a >= b then TrueLit else FalseLit
                                        (e1', e2') -> BinOpExpr meta Gte e1' e2'
  opti (BinOpExpr meta Lte e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a <= b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),  (LiteralExpr _ (CharLit b))) -> LiteralExpr m $ if a <= b then TrueLit else FalseLit
                                        (e1', e2') -> BinOpExpr meta Lte e1' e2'
  opti (BinOpExpr meta Neq e1 e2) = case (opti e1, opti e2) of 
                                        ((LiteralExpr m (IntLit a)),  (LiteralExpr _ (IntLit b))) -> LiteralExpr m $ if a /= b then TrueLit else FalseLit
                                        ((LiteralExpr m (CharLit a)),  (LiteralExpr _ (CharLit b))) -> LiteralExpr m $ if a /= b then TrueLit else FalseLit

                                        ((LiteralExpr m TrueLit),  (LiteralExpr _ TrueLit))  -> LiteralExpr m FalseLit
                                        ((LiteralExpr m FalseLit), (LiteralExpr _ FalseLit)) -> LiteralExpr m FalseLit
                                        ((LiteralExpr m TrueLit),  (LiteralExpr _ FalseLit)) -> LiteralExpr m TrueLit
                                        ((LiteralExpr m FalseLit), (LiteralExpr _ TrueLit))  -> LiteralExpr m TrueLit
                                        
                                        ((LiteralExpr m (TupleLit (a,b))), (LiteralExpr _ (TupleLit (c,d))))  -> LiteralExpr m $ if a == c && b == d then FalseLit else TrueLit  
                                        ((LiteralExpr m (EmptyListLit)), (LiteralExpr _ EmptyListLit))  -> LiteralExpr m FalseLit -- The code gen also does this even for empty lists of type vars


                                        ((VariableExpr _ name),  (VariableExpr _ name')) -> if name == name' then LiteralExpr meta FalseLit else BinOpExpr meta Neq e1 e2
                                        (e1', e2') -> BinOpExpr meta Neq e1' e2'
  -- Other binops
  opti (BinOpExpr m op ex1 ex2) = BinOpExpr m op (opti ex1) (opti ex2)
  opti (UnaryOpExpr meta Min expr) = case opti expr of 
                                              (LiteralExpr m (IntLit lit)) ->  (LiteralExpr m (IntLit (-lit)))
                                              a -> (UnaryOpExpr meta Min a)
  opti (UnaryOpExpr meta Negate expr) = case opti expr of 
                                            (LiteralExpr m TrueLit) -> LiteralExpr m FalseLit
                                            (LiteralExpr m FalseLit) -> LiteralExpr m TrueLit
                                            opti_expr -> (UnaryOpExpr meta Negate opti_expr)
  -- Optimise literal field access
  opti (UnaryOpExpr meta (FieldAccess SecondField) expr) = case opti expr of 
                                            (LiteralExpr _ (TupleLit (_,e2))) -> opti e2 
                                            opti_expr ->  (UnaryOpExpr meta (FieldAccess SecondField) opti_expr)
  opti (UnaryOpExpr meta (FieldAccess FirstField) expr) = case opti expr of 
                                            (LiteralExpr _ (TupleLit (e1,_))) -> opti e1 
                                            opti_expr ->  (UnaryOpExpr meta (FieldAccess FirstField) opti_expr)
  opti (UnaryOpExpr meta (FieldAccess HeadField) expr) = case opti expr of 
                                            (BinOpExpr _ Cons e1 _) -> opti e1
                                            opti_expr ->  (UnaryOpExpr meta (FieldAccess HeadField) opti_expr)
  opti (UnaryOpExpr meta (FieldAccess TailField) expr) = case opti expr of 
                                            (BinOpExpr (ty,_) Cons _ (LiteralExpr (_, m) EmptyListLit)) -> (LiteralExpr (ty, m) EmptyListLit)
                                            (BinOpExpr _ Cons _ e2) -> opti e2 -- Special case for empty list to propagate the type
                                            opti_expr ->  (UnaryOpExpr meta (FieldAccess TailField) opti_expr)
  opti (FunctionCallExpr _ "isEmpty" [LiteralExpr (_, meta) EmptyListLit]) = LiteralExpr (BoolType, meta) TrueLit 
  opti (FunctionCallExpr _ "isEmpty" [BinOpExpr (_,meta) Cons _ _]) = LiteralExpr (BoolType, meta) FalseLit 
  opti (FunctionCallExpr meta funname args) = FunctionCallExpr meta funname (map opti args)

instance Optimise (Stmt TypecheckedP) where
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
  -- Take out expression statements that do not have side effects and just waste cpu
  opti (ExprStmt _ BinOpExpr {}) = BlockStmt [] 
  opti (ExprStmt _ UnaryOpExpr {}) = BlockStmt [] 
  opti (ExprStmt _ VariableExpr {}) = BlockStmt [] 
  opti (ExprStmt _ LiteralExpr {}) = BlockStmt [] 
  opti (ExprStmt meta expr) = ExprStmt meta (opti expr)

instance Optimise (Decl TypecheckedP) where
  opti (FunDecl meta funname retty args funvars body) = FunDecl meta funname retty args (map opti funvars) (map opti body)
  opti (VarDecl meta varname ty expr) = VarDecl meta varname ty (opti expr)

collapseBlocks :: Program p -> Program p
collapseBlocks [] = []
collapseBlocks (var@(VarDecl {}) : later) = var : collapseBlocks later
collapseBlocks (FunDecl a b c d e body : later) = FunDecl a b c d e (collapseBlocksStmt body) : collapseBlocks later

collapseBlocksStmt :: [Stmt p] -> [Stmt p]
collapseBlocksStmt [] = []
collapseBlocksStmt (IfStmt meta condition consequent alternative : later) = IfStmt meta condition (collapseBlocksStmt consequent) (collapseBlocksStmt <$> alternative) : collapseBlocksStmt later
collapseBlocksStmt (WhileStmt meta condition body : later) = WhileStmt meta condition (collapseBlocksStmt body) : collapseBlocksStmt later
collapseBlocksStmt (BlockStmt code : later) = code ++ collapseBlocksStmt later
collapseBlocksStmt (s : later) = s : collapseBlocksStmt later

opti_improvement :: (Show a) => [a] -> [a] -> Int
opti_improvement p1 p2 =
  let length_p1 = length $ show p1
      length_p2 = length $ show p2
      diff = length_p2 - length_p1
   in diff * 100 `div` length_p1

{-

Further improvements:
    - Remove the declerations from functions that have an empty body.
    - Remove the functions that are not called at all
-}
