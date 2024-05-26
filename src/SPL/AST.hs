{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- {-# LANGUAGE DeriveFoldable #-}
module SPL.AST where

-- The only import that we need from here because of the type families.
import SPL.Parser.SourceSpan 
import SPL.Colors (yellow, blue)

{- All the different phases of the compiler -}
data Phase
  = EmptyP        -- Empty phase, used for testing
  | ParsedP       -- Phase after parsing with location information
  | ReturnsCheckedP -- Phase after the retuns have been checked
  | TypecheckedP  -- Phase after full typechecking

type Program (p :: Phase) = [Decl p]

data Type
  = IntType
  | CharType
  | BoolType
  | VoidType
  | TupleType Type Type
  | ListType Type
  | TypeVar { typevarname :: String, rigid :: Bool }
  | FunType [Type] [Type] Type
  deriving (Eq, Ord)

instance Show Type where 
  show IntType = yellow "Int"
  show CharType = yellow "Char"
  show BoolType = yellow "Bool"
  show VoidType = yellow "Void"
  show (TupleType ty1 ty2) = yellow $  "(" ++ show ty1 ++ show ty2 ++ ")"
  show (ListType ty) = yellow $ "[" ++ show ty ++ "]"
  show (TypeVar name False) = yellow $ 'T':'y':'p':'e':'v':'a':'r':' ':blue name 
  show (TypeVar name True) = yellow $ 'R':'i':'g':'i':'d':' ':'T':'y':'p':'e':'v':'a':'r':' ':blue name
  show (FunType types fundecltypes rettype) =  yellow $ listTypes " -> " types ++ show rettype ++ " (with var Decls types {" ++ listTypes "," fundecltypes  ++ "})" 
    where listTypes sep = foldl (\r x -> r ++ show x ++ sep) ""


data Decl (p :: Phase)
  = FunDecl (FunDecl p) String (FunDeclT p) [(String, FunDeclT p)] [Decl p] [Stmt p]
  | VarDecl (VarDecl p) String (VarDeclT p) (Expr p)
deriving instance Eq (Decl EmptyP)
deriving instance Eq (Decl ParsedP)
deriving instance Eq (Decl ReturnsCheckedP)
deriving instance Eq (Decl TypecheckedP)

deriving instance Show (Decl EmptyP)
deriving instance Show (Decl ParsedP)
deriving instance Show (Decl ReturnsCheckedP)
deriving instance Show (Decl TypecheckedP)
-- Closed type family it is thanks https://wiki.haskell.org/GHC/Type_families#Closed_family_simplification we don't need to allow people to extend them and we can always open them up again

type family FunDecl (p :: Phase) where 
  FunDecl EmptyP = ()
  FunDecl ParsedP = SourceSpan
  FunDecl ReturnsCheckedP = SourceSpan
  FunDecl TypecheckedP = SourceSpan

type family FunDeclT (p :: Phase) where
  FunDeclT EmptyP = ()
  FunDeclT ParsedP = Maybe Type
  FunDeclT ReturnsCheckedP = Maybe Type
  FunDeclT TypecheckedP = Type

type family VarDecl (p :: Phase) where
  VarDecl EmptyP = ()
  VarDecl ParsedP = SourceSpan
  VarDecl ReturnsCheckedP = SourceSpan
  VarDecl TypecheckedP = SourceSpan

type family VarDeclT (p :: Phase) where
  VarDeclT EmptyP = ()
  VarDeclT ParsedP = Maybe Type
  VarDeclT ReturnsCheckedP = Maybe Type
  VarDeclT TypecheckedP = Type

data Stmt (p :: Phase) =
  AssignStmt (AssignStmt p) Variable (Expr p)
  | ReturnStmt (ReturnStmt p) (Maybe (Expr p))
  | IfStmt (IfStmt p) (Expr p) [Stmt p] (Maybe [Stmt p])
  | WhileStmt (WhileStmt p) (Expr p) [Stmt p]
  | ExprStmt (ExprStmt p) (Expr p)
  -- W don't parse to a BlockStmt but it is nice if you want to have a list of statements 
  | BlockStmt [Stmt p]
deriving instance Eq (Stmt EmptyP)
deriving instance Eq (Stmt ParsedP)
deriving instance Eq (Stmt ReturnsCheckedP)
deriving instance Eq (Stmt TypecheckedP)

deriving instance Show (Stmt EmptyP)
deriving instance Show (Stmt ParsedP)
deriving instance Show (Stmt ReturnsCheckedP)
deriving instance Show (Stmt TypecheckedP)


type family AssignStmt (p :: Phase) where 
  AssignStmt EmptyP = ()
  AssignStmt ParsedP = SourceSpan
  AssignStmt ReturnsCheckedP = SourceSpan
  AssignStmt TypecheckedP = SourceSpan


type family ReturnStmt (p :: Phase) where
  ReturnStmt EmptyP = ()
  ReturnStmt ParsedP = SourceSpan
  ReturnStmt ReturnsCheckedP = SourceSpan
  ReturnStmt TypecheckedP = SourceSpan

type family IfStmt (p :: Phase) where
  IfStmt EmptyP = ()
  IfStmt ParsedP = SourceSpan
  IfStmt ReturnsCheckedP = SourceSpan
  IfStmt TypecheckedP = SourceSpan

type family WhileStmt (p :: Phase) where
  WhileStmt EmptyP = ()
  WhileStmt ParsedP = SourceSpan
  WhileStmt ReturnsCheckedP = SourceSpan
  WhileStmt TypecheckedP = SourceSpan

type family ExprStmt (p :: Phase) where
  ExprStmt EmptyP = ()
  ExprStmt ParsedP = SourceSpan
  ExprStmt ReturnsCheckedP = SourceSpan
  ExprStmt TypecheckedP = SourceSpan

type family VarStmt (p :: Phase) where
  VarStmt EmptyP = ()
  VarStmt ParsedP = SourceSpan
  VarStmt ReturnsCheckedP = SourceSpan
  VarStmt TypecheckedP = SourceSpan

data UnaryOp = Negate | FieldAccess Field
  deriving (Eq, Show)

data BinOp =
  Mul | Div | Mod | Add |
  Sub | Cons | Gt | Gte |
  Lt | Lte | Eq | Neq |
  And | Or
  deriving (Eq, Show)

data Expr (p :: Phase) =
  BinOpExpr (BinOpExpr p) BinOp (Expr p) (Expr p)
  | UnaryOpExpr (UnaryOpExpr p) UnaryOp (Expr p)
  | FunctionCallExpr (FunctionCallExpr p) String [Expr p]
  | VariableExpr (VariableExpr p) Variable
  | LiteralExpr (LiteralExpr p) (Literal p)
deriving instance Eq (Expr EmptyP)
deriving instance Eq (Expr ParsedP)
deriving instance Eq (Expr ReturnsCheckedP)
deriving instance Eq (Expr TypecheckedP)

deriving instance Show (Expr EmptyP)
deriving instance Show (Expr ParsedP)
deriving instance Show (Expr ReturnsCheckedP)
deriving instance Show (Expr TypecheckedP)

type family BinOpExpr (p :: Phase) where
  BinOpExpr EmptyP = ()
  BinOpExpr ParsedP = SourceSpan
  BinOpExpr ReturnsCheckedP = SourceSpan
  BinOpExpr TypecheckedP = SourceSpan


type family UnaryOpExpr (p :: Phase) where
  UnaryOpExpr EmptyP = ()
  UnaryOpExpr ParsedP = SourceSpan
  UnaryOpExpr ReturnsCheckedP = SourceSpan
  UnaryOpExpr TypecheckedP = SourceSpan

type family FunctionCallExpr (p :: Phase) where
  FunctionCallExpr EmptyP = ()
  FunctionCallExpr ParsedP = SourceSpan
  FunctionCallExpr ReturnsCheckedP = SourceSpan
  FunctionCallExpr TypecheckedP = SourceSpan

type family VariableExpr (p :: Phase) where
  VariableExpr EmptyP = ()
  VariableExpr ParsedP = SourceSpan
  VariableExpr ReturnsCheckedP = SourceSpan
  VariableExpr TypecheckedP = SourceSpan

type family LiteralExpr (p :: Phase) where
  LiteralExpr EmptyP = ()
  LiteralExpr ParsedP = SourceSpan
  LiteralExpr ReturnsCheckedP = SourceSpan
  LiteralExpr TypecheckedP = SourceSpan

data Literal (p :: Phase) =
  TrueLit
  | FalseLit
  | IntLit Int
  | CharLit Char
  | TupleLit (Expr p, Expr p)
  | EmptyListLit
deriving instance Eq (Literal EmptyP)
deriving instance Eq (Literal ParsedP)
deriving instance Eq (Literal ReturnsCheckedP)
deriving instance Eq (Literal TypecheckedP)

deriving instance Show (Literal EmptyP)
deriving instance Show (Literal ParsedP)
deriving instance Show (Literal ReturnsCheckedP)
deriving instance Show (Literal TypecheckedP)


data Field = HeadField | TailField
  deriving (Eq, Show)

data Variable
  = Identifier String (Maybe Field)
  deriving (Eq, Show)

{- Start of type familiy instance convertable -}
class Convertable n (p1 :: Phase) (p2 :: Phase) where
  -- We use this to explicitly change the phase of a node in a case that it won't fail anyway
  -- convert :: n p1 -> n p2
  upgrade :: n p1 -> n p2 -- Same name but upgrade makes more sense because your moving on 
  -- This is great because it works if the shape is the same!

instance Convertable Expr ParsedP ReturnsCheckedP where
  upgrade (BinOpExpr meta op e1 e2) = BinOpExpr meta op (upgrade e1) (upgrade e2)
  upgrade (UnaryOpExpr meta op e) = UnaryOpExpr meta op (upgrade e)
  upgrade (FunctionCallExpr meta name exprs) = FunctionCallExpr meta name (map upgrade exprs)
  upgrade (VariableExpr meta var) = VariableExpr meta var
  upgrade (LiteralExpr meta lit) = LiteralExpr meta (upgrade lit)

instance Convertable Literal ParsedP ReturnsCheckedP where
  upgrade (TupleLit (e1, e2)) = TupleLit (upgrade e1, upgrade e2)
  upgrade TrueLit = TrueLit
  upgrade FalseLit = FalseLit
  upgrade (IntLit i) = IntLit i
  upgrade (CharLit c) = CharLit c
  upgrade EmptyListLit = EmptyListLit

instance Convertable Stmt ParsedP ReturnsCheckedP where
  upgrade (AssignStmt meta var e) = AssignStmt meta var (upgrade e)
  upgrade (ReturnStmt meta (Just e)) = ReturnStmt meta (Just (upgrade e))
  upgrade (ReturnStmt meta Nothing) = ReturnStmt meta Nothing
  upgrade (IfStmt meta e body Nothing) = IfStmt meta (upgrade e) (map upgrade body) Nothing
  upgrade (IfStmt meta e body (Just alternative)) = IfStmt meta (upgrade e) (map upgrade body) (Just (map upgrade alternative))
  upgrade (WhileStmt meta e body) = WhileStmt meta (upgrade e) (map upgrade body)
  upgrade (ExprStmt meta e) = ExprStmt meta (upgrade e)
  upgrade (BlockStmt b) = BlockStmt (map upgrade b)

instance Convertable Decl ParsedP ReturnsCheckedP where
  upgrade (VarDecl meta name ty e) = VarDecl meta name ty (upgrade e)
  upgrade (FunDecl meta name retty args funvars body) = FunDecl meta name retty args (map upgrade funvars) (map upgrade body)

{- TypecheckedP instaces-}

-- type instance BinOpExpr TypecheckedP = (SourceSpan, Type)
-- type instance UnaryOpExpr TypecheckedP = (SourceSpan, Type)
-- type instance FunctionCallExpr TypecheckedP = (SourceSpan, Type)
-- type instance VariableExpr TypecheckedP = (SourceSpan, Type)
-- type instance LiteralExpr TypecheckedP = (SourceSpan, Type)


-- Convertable to upgrade ParsedP to TypecheckedP

instance Convertable Expr ReturnsCheckedP TypecheckedP where
  upgrade (BinOpExpr meta op e1 e2) = BinOpExpr (meta :: BinOpExpr TypecheckedP) op (upgrade e1) (upgrade e2)
  upgrade (UnaryOpExpr meta op e) = UnaryOpExpr meta op (upgrade e)
  upgrade (FunctionCallExpr meta name exprs) = FunctionCallExpr meta name (map upgrade exprs)
  upgrade (VariableExpr meta var) = VariableExpr meta var
  upgrade (LiteralExpr meta lit) = LiteralExpr meta (upgrade lit)

instance Convertable Literal ReturnsCheckedP TypecheckedP where
  upgrade (TupleLit (e1, e2)) = TupleLit (upgrade e1, upgrade e2)
  upgrade TrueLit = TrueLit
  upgrade FalseLit = FalseLit
  upgrade (IntLit i) = IntLit i
  upgrade (CharLit c) = CharLit c
  upgrade EmptyListLit = EmptyListLit

instance Convertable Stmt ReturnsCheckedP TypecheckedP where
  upgrade (AssignStmt meta var e) = AssignStmt (meta :: AssignStmt TypecheckedP) var (upgrade e)
  upgrade (ReturnStmt meta (Just e)) = ReturnStmt (meta :: ReturnStmt TypecheckedP) (Just (upgrade e))
  upgrade (ReturnStmt meta Nothing) = ReturnStmt (meta :: ReturnStmt TypecheckedP) Nothing
  upgrade (IfStmt meta e body Nothing) = IfStmt (meta :: IfStmt TypecheckedP) (upgrade e) (map upgrade body) Nothing
  upgrade (IfStmt meta e body (Just alternative)) = IfStmt (meta :: IfStmt TypecheckedP) (upgrade e) (map upgrade body) (Just (map upgrade alternative))
  upgrade (WhileStmt meta e body) = WhileStmt (meta :: WhileStmt TypecheckedP) (upgrade e) (map upgrade body)
  upgrade (ExprStmt meta e) = ExprStmt (meta :: ExprStmt TypecheckedP) (upgrade e)
  upgrade (BlockStmt list) = BlockStmt (map upgrade list) 


{- EmptyP instaces-}
class Emptiable n where
  -- Converts an AST node to the unannotated version of that node, special case of Convertabl
  empty :: n p -> n EmptyP


instance Emptiable Expr where
  empty (BinOpExpr _ op left right) = BinOpExpr () op (empty left) (empty right)
  empty (UnaryOpExpr _ op expr) = UnaryOpExpr () op (empty expr)
  empty (FunctionCallExpr _ name args) = FunctionCallExpr () name (empty <$> args)
  empty (VariableExpr _ variable) = VariableExpr () variable
  empty (LiteralExpr _ literal) = LiteralExpr () (empty literal)

instance Emptiable Decl where
  -- Emptying the type here is not ideal, but it seems to be the only way with type families
  empty (FunDecl _ name _ args decls body) = FunDecl () name () (map (\(n, _) -> (n, ())) args) (empty <$> decls) (empty <$> body)
  empty (VarDecl _ name _ val) = VarDecl () name () (empty val)

instance Emptiable Stmt where
  empty (AssignStmt _ variable val) = AssignStmt () variable (empty val)
  empty (ReturnStmt _ expr) = ReturnStmt () (empty <$> expr)
  empty (IfStmt _ condition consequent alternative) = IfStmt () (empty condition) (map empty consequent) (map empty <$> alternative)
  empty (WhileStmt _ condition body) = WhileStmt () (empty condition) (empty <$> body)
  empty (ExprStmt _ expr) = ExprStmt () (empty expr)
  empty (BlockStmt body) = BlockStmt (empty <$> body)

instance Emptiable Literal where
  empty TrueLit = TrueLit
  empty FalseLit = FalseLit
  empty (IntLit i) = IntLit i
  empty (CharLit c) = CharLit c
  empty (TupleLit (e1, e2)) = TupleLit (empty e1, empty e2)
  empty EmptyListLit = EmptyListLit