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
    FunDecl Identifier (Maybe Type) [(Identifier, Maybe Type)] [Stmt]
    -- var hello = 'w':'o':'r':'l':'d':[]
  | VarDecl (Maybe Type) Identifier Expr
  deriving (Eq, Show)

data Stmt =
      ReturnStmt (Maybe Expr)            -- return a;
    | IfStmt Expr [Stmt] (Maybe [Stmt])  -- if (a) {b} else {c}
    | WhileStmt Expr [Stmt]              -- while (a) {b}
    | ExprStmt Expr                      -- a;
    deriving (Eq, Show)
  
data Expr =
      BinOp BinOp Expr Expr          -- a ∘ b
    | UnaryOp UnaryOp Expr           -- ∘ a
    | AssignExpr Variable Expr       -- a = b
    | FunctionCall Identifier [Expr] -- f()
    | VariableExpr Variable          -- a
    | LiteralExpr Literal            -- 10
    deriving (Eq, Show)

data UnaryOp = Negate -- !a
  deriving (Eq, Show)

data BinOp =
      Mul   -- '*'
    | Div   -- '/'
    | Mod   -- '%'
    | Add   -- '+'
    | Sub   -- '-'
    | Cons  -- ':'
    | Gt    -- '>'
    | Gte   -- '>='
    | Lt    -- '<'
    | Lte   -- '<='
    | Eq    -- '=='
    | Neq   -- '!='
    | And   -- '&&'
    | Or    -- '||'
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

