{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module SPL.Parser.AST where

class Emptiable n where
  -- Converts an AST node to the unannotated version of that node
  empty :: n p -> n EmptyP

data Phase
  = EmptyP
  | ParsedP
  | TypecheckedP

type Program (p :: Phase) = [Decl p]

data Type =
  IntType
  | CharType
  | BoolType
  | VoidType
  | TupleType Type Type
  | ListType Type
  | TypeVar String
  deriving (Eq, Show)

data UnaryOp = Negate | FieldAccess Field
  deriving (Eq, Show)

data BinOp =
  Mul | Div | Mod | Add |
  Sub | Cons | Gt | Gte |
  Lt | Lte | Eq | Neq |
  And | Or
  deriving (Eq, Show)

data Field = HeadField | TailField
  deriving (Eq, Show)

data Decl (p :: Phase) =
  FunDecl (FunDecl p) String (Maybe Type) [(String, Maybe Type)] [Stmt p]
  | VarDecl (VarDecl p) String (Maybe Type) (Maybe (Expr p))
deriving instance Eq (Decl EmptyP)
deriving instance Show (Decl EmptyP)

type family FunDecl (p :: Phase)
type instance FunDecl EmptyP = ()

type family VarDecl (p :: Phase)
type instance VarDecl EmptyP = ()

instance Emptiable Decl where
  empty (FunDecl _ name ty args body) = FunDecl () name ty args (empty <$> body)
  empty (VarDecl _ name ty val) = VarDecl () name ty (empty <$> val)

data Stmt (p :: Phase) =
  ReturnStmt (ReturnStmt p) (Maybe (Expr p))
  | IfStmt (IfStmt p) (Expr p) [Stmt p] (Maybe [Stmt p])
  | WhileStmt (WhileStmt p) (Expr p) [Stmt p]
  | ExprStmt (ExprStmt p) (Expr p)
  | VarStmt (VarStmt p) (Maybe Type) String (Expr p)
deriving instance Eq (Stmt EmptyP)
deriving instance Show (Stmt EmptyP)

instance Emptiable Stmt where
  empty (ReturnStmt _ stmt) = ReturnStmt () (empty <$> stmt)
  empty (IfStmt _ condition consequent alternative) = IfStmt () (empty condition) (map empty consequent) (map empty <$> alternative)
  empty (WhileStmt _ condition body) = WhileStmt () (empty condition) (empty <$> body)
  empty (ExprStmt _ expr) = ExprStmt () (empty expr)
  empty (VarStmt _ ty name val) = VarStmt () ty name (empty val)

type family ReturnStmt (p :: Phase)
type instance ReturnStmt EmptyP = ()

type family IfStmt (p :: Phase)
type instance IfStmt EmptyP = ()

type family WhileStmt (p :: Phase)
type instance WhileStmt EmptyP = ()

type family ExprStmt (p :: Phase)
type instance ExprStmt EmptyP = ()

type family VarStmt (p :: Phase)
type instance VarStmt EmptyP = ()

data Expr (p :: Phase) =
  BinOpExpr (BinOpExpr p) BinOp (Expr p) (Expr p)
  | UnaryOpExpr (UnaryOpExpr p) UnaryOp (Expr p)
  | AssignExpr (AssignExpr p) Variable (Expr p)
  | FunctionCallExpr (FunctionCallExpr p) String [Expr p]
  | VariableExpr (VariableExpr p) Variable
  | LiteralExpr (LiteralExpr p) (Literal p)
deriving instance Eq (Expr EmptyP)
deriving instance Show (Expr EmptyP)

instance Emptiable Expr where
  empty (BinOpExpr _ op left right) = BinOpExpr () op (empty left) (empty right)
  empty (UnaryOpExpr _ op expr) = UnaryOpExpr () op (empty expr)
  empty (AssignExpr _ variable val) = AssignExpr () variable (empty val)
  empty (FunctionCallExpr _ name args) = FunctionCallExpr () name (empty <$> args)
  empty (VariableExpr _ variable) = VariableExpr () variable
  empty (LiteralExpr _ literal) = LiteralExpr () (empty literal)

type family BinOpExpr (p :: Phase)
type instance BinOpExpr EmptyP = ()

type family UnaryOpExpr (p :: Phase)
type instance UnaryOpExpr EmptyP = ()

type family AssignExpr (p :: Phase)
type instance AssignExpr EmptyP = ()

type family FunctionCallExpr (p :: Phase)
type instance FunctionCallExpr EmptyP = ()

type family VariableExpr (p :: Phase)
type instance VariableExpr EmptyP = ()

type family LiteralExpr (p :: Phase)
type instance LiteralExpr EmptyP = ()

data Literal (p :: Phase) =
  TrueLit
  | FalseLit
  | IntLit Int
  | FloatLit Float
  | CharLit Char
  | TupleLit (Expr p, Expr p)
  | EmptyListLit
deriving instance Eq (Literal EmptyP)
deriving instance Show (Literal EmptyP)

instance Emptiable Literal where
  empty TrueLit = TrueLit
  empty FalseLit = FalseLit
  empty (IntLit i) = IntLit i
  empty (FloatLit f) = FloatLit f
  empty (CharLit c) = CharLit c
  empty (TupleLit (e1, e2)) = TupleLit (empty e1, empty e2)
  empty EmptyListLit = EmptyListLit

data Variable
  = Identifier String (Maybe Field)
  deriving (Eq, Show)
