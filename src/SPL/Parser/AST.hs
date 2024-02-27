{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SPL.Parser.AST where

type Program = [Decl]

data Location = Location
  { line :: Int
  , column :: Int
  } deriving (Eq, Show)

data Annotation = Annotation
  { location :: Location
  } deriving (Eq, Show)
type Annotated a = (Annotation, a)

class Annotate f a where
  annotate :: a f -> Annotation -> f

data TypeF r =
  IntType
  | CharType
  | BoolType
  | VoidType
  | TupleType r r
  | ListType r
  | TypeVar String
  deriving (Eq, Show)

data DeclF = FunDecl String (Maybe Type) [(String, Maybe Type)] [Stmt]
  deriving (Eq, Show)

data StmtF r =
  ReturnStmt (Maybe Expr)
  | IfStmt Expr [r] (Maybe [r])
  | WhileStmt Expr [r]
  | ExprStmt Expr
  | VarStmt (Maybe Type) String Expr
  deriving (Eq, Show)

data ExprF r =
  BinOpExpr BinOp r r
  | UnaryOpExpr UnaryOp r
  | AssignExpr Variable r
  | FunctionCallExpr String [r]
  | VariableExpr Variable
  | LiteralExpr Literal
  deriving (Eq, Show)

data UnaryOp = 
  Negate              -- !a
  | FieldAccess Field -- .hd, .tl
  deriving (Eq, Show)

data BinOp =
    Mul    -- '*'
    | Div  -- '/'
    | Mod  -- '%'
    | Add  -- '+'
    | Sub  -- '-'
    | Cons -- ':'
    | Gt   -- '>'
    | Gte  -- '>='
    | Lt   -- '<'
    | Lte  -- '<='
    | Eq   -- '=='
    | Neq  -- '!='
    | And  -- '&&'
    | Or   -- '||'
    deriving (Eq, Show)

data Field = 
  HeadField   -- hd
  | TailField -- tl
  deriving (Eq, Show)

data Literal =
  TrueLit                 -- true
  | FalseLit              -- false
  | IntLit Int            -- 10, -10, +10
  | FloatLit Float        -- 10.0, -10.0, +10.0
  | CharLit Char          -- 'a'
  | TupleLit (Expr, Expr) -- (a, b)
  | EmptyListLit          -- []
  deriving (Eq, Show)

data Variable = Identifier String (Maybe Field) -- a, a.hd, a.tl
  deriving (Eq, Show)

newtype Type = Type (Annotated (TypeF Type)) deriving (Eq, Show)
newtype Decl = Decl (Annotated DeclF) deriving (Eq, Show)
newtype Stmt = Stmt (Annotated (StmtF Stmt)) deriving (Eq, Show)
newtype Expr = Expr (Annotated (ExprF Expr)) deriving (Eq, Show)

instance Annotate Stmt StmtF where
  annotate :: StmtF Stmt -> Annotation -> Stmt
  annotate n a = Stmt (a, n)
