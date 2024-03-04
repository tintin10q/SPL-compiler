{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE InstanceSigs #-}
module SPL.Parser.AST where

import SPL.Parser.Parser (Parser)

import Text.Megaparsec (SourcePos, getSourcePos)

type Program = [Decl]

data Location = Location
  { start :: SourcePos
  , end :: SourcePos
  } deriving (Eq, Show)
data Annotation = Annotation
  { location :: Location
  } deriving (Eq, Show)
type Annotated a = (Annotation, a)

class Annotate u a where
  -- Converts an unannotated node "u" to an annotated node "a".
  annotate :: u -> Annotation -> a

-- Converts a parser for an unannotated node to a parser for an annotated node.
annotated :: (Annotate u a) => Parser u -> Parser a
annotated parser = do
  startPos <- getSourcePos
  node     <- parser
  endPos   <- getSourcePos
  return $ annotate node $ mkAnnotation startPos endPos

-- Extendable constructor for Annotation
mkAnnotation :: SourcePos -> SourcePos -> Annotation
mkAnnotation s e = Annotation $ Location s e

data TypeF r =
  IntType
  | CharType
  | BoolType
  | VoidType
  | TupleType r r
  | ListType r
  | TypeVar String
  deriving (Eq, Show)

type TypeU = TypeF Type
newtype Type = Type (Annotated TypeU) deriving (Eq, Show)

instance Annotate TypeU Type where
  annotate :: TypeU -> Annotation -> Type
  annotate node annotation = Type (annotation, node)

data DeclF = FunDecl String (Maybe Type) [(String, Maybe Type)] [Stmt]
  deriving (Eq, Show)

type DeclU = DeclF
newtype Decl = Decl (Annotated DeclU) deriving (Eq, Show)

instance Annotate DeclU Decl where
  annotate :: DeclU -> Annotation -> Decl
  annotate node annotation = Decl (annotation, node)

data StmtF r =
  ReturnStmt (Maybe Expr)
  | IfStmt Expr [r] (Maybe [r])
  | WhileStmt Expr [r]
  | ExprStmt Expr
  | VarStmt (Maybe Type) String Expr
  deriving (Eq, Show)

type StmtU = StmtF Stmt
newtype Stmt = Stmt (Annotated StmtU) deriving (Eq, Show)

instance Annotate StmtU Stmt where
  annotate :: StmtU -> Annotation -> Stmt
  annotate node annotation = Stmt (annotation, node)

data ExprF r =
  BinOpExpr BinOp r r
  | UnaryOpExpr UnaryOp r
  | AssignExpr Variable r
  | FunctionCallExpr String [r]
  | VariableExpr Variable
  | LiteralExpr Literal
  deriving (Eq, Show)

type ExprU = ExprF Expr
newtype Expr = Expr (Annotated ExprU) deriving (Eq, Show)

instance Annotate ExprU Expr where
  annotate :: ExprU -> Annotation -> Expr
  annotate node annotation = Expr (annotation, node)

data LiteralF =
  TrueLit
  | FalseLit
  | IntLit Int
  | FloatLit Float
  | CharLit Char
  | TupleLit (Expr, Expr)
  | EmptyListLit
  deriving (Eq, Show)

type LiteralU = LiteralF
newtype Literal = Literal (Annotated LiteralU) deriving (Eq, Show)

instance Annotate LiteralU Literal where
  annotate :: LiteralU -> Annotation -> Literal
  annotate node annotation = Literal (annotation, node)

data VariableF = Identifier String (Maybe Field)
  deriving (Eq, Show)

type VariableU = VariableF
newtype Variable = Variable (Annotated VariableU) deriving (Eq, Show)

instance Annotate VariableU Variable where
  annotate :: VariableU -> Annotation -> Variable
  annotate node annotation = Variable (annotation, node)

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

