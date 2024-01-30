module Parser.AST where

type Identifier = String
type Program = [Statement]

data Statement = TODO

data Expr =
      BinOp BinOp Expr Expr
    | UnaryOp UnaryOp Expr
    | AssignExpr Variable Expr
    | VariableExpr Variable
    | FunctionCall Identifier [Expr]
    | LiteralExpr Literal

data UnaryOp =
      Neg -- -a, !a
    | Pos -- +a

data BinOp =
      Gt    -- a > b
    | Gte   -- a >= b
    | Lt    -- a < b
    | Lte   -- a <= b
    | Eq    -- a == b
    | Neq   -- a != b
    | And   -- a && b
    | Or    -- a || b
    | Imp   -- a -> b
    | Xor   -- a xor b
    | Add   -- a + b
    | Sub   -- a - b
    | Mod   -- a % b
    | Div   -- a / b
    | Exp   -- a ^ b
    | Cons  -- a:b

data Variable = 
      Variable Identifier -- a
    | Property Identifier Variable -- a.b

data Literal =
      True
    | False
    | Int Int
    | Float Float
    | Char Char
    | EmptyList
