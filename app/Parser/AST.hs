module Parser.AST where

type Program = [Decl]

data Type =
      IntType             -- Int
    | CharType            -- Char
    | BoolType            -- Bool
    | VoidType            -- Void
    | TupleType Type Type -- (a, b)
    | ListType Type       -- [a]
    | TypeVar String      -- a
    deriving (Eq, Show)

data Decl =
    -- a(b : c, d : e) : Bool {
    --     f();
    -- }
    FunDecl String (Maybe Type) [(String, Maybe Type)] [Stmt]
  deriving (Eq, Show)

data Stmt =
      ReturnStmt (Maybe Expr)           -- return a;
    | IfStmt Expr [Stmt] (Maybe [Stmt]) -- if (a) {b} else {c}
    | WhileStmt Expr [Stmt]             -- while (a) {b}
    | ExprStmt Expr                     -- a;
    | VarStmt (Maybe Type) String Expr  -- var hello = 'w':'o':'r':'l':'d':[]
    deriving (Eq, Show)
  
data Expr =
      BinOp BinOp Expr Expr      -- a ∘ b
    | UnaryOp UnaryOp Expr       -- ∘ a
    | AssignExpr Variable Expr   -- a = b
    | FunctionCall String [Expr] -- f()
    | VariableExpr Variable      -- a, a.tl, a.hd
    | LiteralExpr Literal        -- 10
    deriving (Eq, Show)

data UnaryOp = 
    Negate            -- !a
  | FieldAccess Field -- .hd, .tl
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
      Identifier String (Maybe Field) -- a, a.hd, a.tl
    deriving (Eq, Show)

data Field =
      HeadField -- hd
    | TailField -- tl
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

