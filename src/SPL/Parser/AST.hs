{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE DeriveFoldable #-}
module SPL.Parser.AST where

class Convertable n (p1 :: Phase) (p2 :: Phase) where
  -- We use this to explicitly change the phase of a node in a case that it won't fail anyway
  convert :: n p1 -> n p2

class Emptiable n where
  -- Converts an AST node to the unannotated version of that node, special case of Convertabl
  empty :: n p -> n EmptyP

data Phase
  = EmptyP        -- Empty phase, used for testing
  | ParsedP       -- Phase after parsing with location information
  | InstantiatedP -- Phase after fully instantiating all type variables
  | TypecheckedP  -- Phase after full typechecking

type Program (p :: Phase) = [Decl p]

data Type
  = IntType
  | CharType
  | BoolType
  | VoidType
  | TupleType Type Type
  | ListType Type
  | TypeVar { name :: String, rigid :: Bool }
  | FunType [Type] Type
  deriving (Eq, Show, Ord)


data UnaryOp = Negate | FieldAccess Field
  deriving (Eq, Show)

data BinOp =
  Mul | Div | Mod | Add |
  Sub | Cons | Gt | Gte |
  Lt | Lte | Eq | Neq |
  And | Or
  deriving (Eq, Show)


data Decl (p :: Phase)
  = FunDecl (FunDecl p) String (FunDeclT p) [(String, FunDeclT p)] [FunVarDecl p] [Stmt p]
  | VarDecl (VarDecl p) String (VarDeclT p) (Expr p)
deriving instance Eq (Decl EmptyP)
deriving instance Show (Decl EmptyP)

type family FunDecl (p :: Phase)
type instance FunDecl EmptyP = ()

type family FunDeclT (p :: Phase)
type instance FunDeclT EmptyP = ()

type family VarDecl (p :: Phase)
type instance VarDecl EmptyP = ()

type family VarDeclT (p :: Phase)
type instance VarDeclT EmptyP = ()

instance Emptiable Decl where
  -- Emptying the type here is not ideal, but it seems to be the only way with type families
  empty (FunDecl _ name _ args decls body) = FunDecl () name () (map (\(n, _) -> (n, ())) args) (empty <$> decls) (empty <$> body)
  empty (VarDecl _ name _ val) = VarDecl () name () (empty val)

data FunVarDecl p = FunVarDeclConstr (FunVarDeclConstr p) String (FunVarDeclT p) (Expr p) 
deriving instance Eq (FunVarDecl EmptyP)
deriving instance Show (FunVarDecl EmptyP)

type family FunVarDeclConstr (p :: Phase)
type instance FunVarDeclConstr EmptyP = ()

instance Emptiable FunVarDecl where
  empty :: FunVarDecl p -> FunVarDecl EmptyP
  empty (FunVarDeclConstr meta name _ e) = FunVarDeclConstr () name () (empty e) 

type family FunVarDeclT (p :: Phase)
type instance FunVarDeclT EmptyP = ()

data Stmt (p :: Phase) =
  ReturnStmt (ReturnStmt p) (Maybe (Expr p))
  | IfStmt (IfStmt p) (Expr p) [Stmt p] (Maybe [Stmt p])
  | WhileStmt (WhileStmt p) (Expr p) [Stmt p]
  | ExprStmt (ExprStmt p) (Expr p)
deriving instance Eq (Stmt EmptyP)
deriving instance Show (Stmt EmptyP)


instance Emptiable Stmt where
  empty (ReturnStmt _ expr) = ReturnStmt () (empty <$> expr)
  empty (IfStmt _ condition consequent alternative) = IfStmt () (empty condition) (map empty consequent) (map empty <$> alternative)
  empty (WhileStmt _ condition body) = WhileStmt () (empty condition) (empty <$> body)
  empty (ExprStmt _ expr) = ExprStmt () (empty expr)

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
  | CharLit Char
  | TupleLit (Expr p, Expr p)
  | EmptyListLit
deriving instance Eq (Literal EmptyP)
deriving instance Show (Literal EmptyP)

instance Emptiable Literal where
  empty TrueLit = TrueLit
  empty FalseLit = FalseLit
  empty (IntLit i) = IntLit i
  empty (CharLit c) = CharLit c
  empty (TupleLit (e1, e2)) = TupleLit (empty e1, empty e2)
  empty EmptyListLit = EmptyListLit


data Field = HeadField | TailField
  deriving (Eq, Show)

data Variable
  = Identifier String (Maybe Field)
  deriving (Eq, Show)


