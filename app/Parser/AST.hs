module Parser.AST where

type Identifier = String
type Program = [Decl]

data Type =
      IntType             -- Int
    | CharType            -- Char
    | BoolType            -- Bool
    | VoidType            -- Void
    | TupleType Type Type -- (a, b)
    | ListType Type       -- [a]
    | TypeVar Identifier  -- a
    deriving (Eq, Show)

data Decl = 
      -- a(b : c, d : e) : Bool {
      --     f();
      -- }
      FunctionDecl Identifier (Maybe Type) [(Identifier, Maybe Type)] [Stmt]
    deriving (Eq, Show)

data Stmt =
      ReturnStmt Expr                    -- return a;
    | IfStmt Expr Stmt (Maybe Stmt)      -- if (a) {b} else {c}
    | WhileStmt Expr Stmt                -- while (a) {b}
    | ExprStmt Expr                      -- a;
    | BlockStmt [Stmt]                   -- { a }
    | VarStmt (Maybe Type) Variable Expr -- a b = c;
    deriving (Eq, Show)

data Expr =
      BinOp BinOp Expr Expr          -- a ∘ b
    | UnaryOp UnaryOp Expr           -- ∘ a
    | AssignExpr Variable Expr       -- a = b
    | FunctionCall Identifier [Expr] -- f()
    | VariableExpr Variable          -- a
    | LiteralExpr Literal            -- 10
    deriving (Eq, Show)

data UnaryOp =
    Negate   -- !a
    deriving (Eq, Show)

data BinOp =
--  | Exp   -- a ^ b
    --
      Mul   -- a * b
    | Div   -- a / b
    | Mod   -- a % b
    --
    | Add   -- a + b
    | Sub   -- a - b
    --
    | Cons  -- a:b
    --
    | Gt    -- a > b
    | Gte   -- a >= b
    | Lt    -- a < b
    | Lte   -- a <= b
    | Eq    -- a == b
    | Neq   -- a != b
    --
    | And   -- a && b
    --
    | Or    -- a || b
    --
--  | Xor   -- a xor b
    --
--  | Imp   -- a -> b
    deriving (Eq, Show)

data Variable = 
      Identifier Identifier -- a
    | Property Expr Identifier -- a.b
    deriving (Eq, Show)

data Literal =
      TrueLit               -- true
    | FalseLit              -- false
    | IntLit Int            -- 10, -10, +10
    | FloatLit Float        -- 10.0, -10.0, +10.0
    | CharLit Char          -- 'a'
    | TupleLit (Expr, Expr) -- (a, b)
    | EmptyListLit          -- []
    deriving (Eq, Show)
