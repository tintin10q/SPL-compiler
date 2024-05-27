{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module SPL.PrettyPrint where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import SPL.AST
import SPL.Colors
import Text.PrettyPrint (render, text, vcat)

-- Pretty print function for Map
prettyPrintMap :: (Show k, Show v) => Map k v -> String
prettyPrintMap m = render $ vcat $ map formatEntry (Map.toList m)
  where
    formatEntry (k, v) = text (show k) <> text " :: " <> text (show v)

-- Function to print the list with commas, excluding the last element
printWithCommas :: [String] -> String
printWithCommas [] = ""
printWithCommas [x] = x
printWithCommas [x, lst] = x ++ " and " ++ lst
printWithCommas (x : xs) = x ++ (if null xs then "" else ", ") ++ printWithCommas xs

class Prettier a where
  {-# MINIMAL pretty #-}
  pretty :: a -> String
  prettyBrief :: a -> String
  prettyBrief = pretty

instance Prettier (Program ParsedP) where
  pretty [] = ""
  pretty (d1@VarDecl {} : later@(VarDecl {} : _)) = pretty d1 ++ pretty later
  pretty (d1@VarDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n"  ++ pretty later
  pretty (d1@FunDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n\n" ++ pretty later
  pretty (d : later) = pretty d ++ pretty later
  prettyBrief [] = ""
  prettyBrief (d : later) = prettyBrief d ++ prettyBrief later

instance Prettier (Program ReturnsCheckedP) where
  pretty [] = ""
  pretty (d1@VarDecl {} : later@(VarDecl {} : _)) = pretty d1 ++ pretty later
  pretty (d1@VarDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n"  ++ pretty later
  pretty (d1@FunDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n\n" ++ pretty later
  pretty (d : later) = pretty d  ++ pretty later
  prettyBrief [] = ""
  prettyBrief (d : later) = prettyBrief d ++ prettyBrief later

instance Prettier (Program TypecheckedP) where
  pretty [] = "\n"
  pretty (d1@VarDecl {} : later@(VarDecl {} : _)) = pretty d1 ++ pretty later
  pretty (d1@VarDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n"  ++ pretty later
  pretty (d1@FunDecl {} : later@(FunDecl {} : _)) = pretty d1 ++ "\n\n" ++ pretty later
  pretty (d : later) = pretty d ++ pretty later
  prettyBrief [] = "\n"
  prettyBrief (d : later) = prettyBrief d ++ prettyBrief later

instance Prettier (Decl ParsedP) where
  pretty var@VarDecl {} = pretty (upgrade var :: Decl ReturnsCheckedP)
  pretty fun@FunDecl {} = pretty (upgrade fun :: Decl ReturnsCheckedP)
  prettyBrief var@VarDecl {} = prettyBrief (upgrade var :: Decl ReturnsCheckedP)
  prettyBrief fun@FunDecl {} = prettyBrief (upgrade fun :: Decl ReturnsCheckedP)

instance Prettier (Decl ReturnsCheckedP) where
  pretty (VarDecl _ name ty expr) = maybe "var" pretty ty ++ ' ' : blue name ++ " = " ++ pretty expr ++ ";\n"
  pretty (FunDecl _ name retty args funvars body) =
    blue name
      ++ "("
      ++ pretty args
      ++ ")"
      ++ pretty retty
      ++ " {\n"
      ++ (if not $ null funvars then indent $ pretty funvars else "")
      ++ pretty body
      ++ "}"
  prettyBrief (FunDecl _ name retty args funvars body) =
    "\n\n" ++
      blue name
      ++ "("
      ++ prettyBrief args
      ++ ")"
      ++ prettyBrief retty
      ++ " {\n"
      ++ (if not $ null funvars then indent $ prettyBrief funvars else "")
      ++ indent (prettyBrief body)
      ++ "\n}"
  prettyBrief var@VarDecl {} = pretty var

instance Prettier [(String, Maybe Type)] where
  pretty = intercalate ", " . map pretty

instance Prettier (String, Maybe Type) where
  pretty (name', Nothing) = blue name'
  pretty (name', Just ty) = blue name' ++ " : " ++ pretty ty

instance Prettier (Maybe Type) where
  pretty Nothing = ""
  pretty (Just ty) = " : " ++ pretty ty

instance Prettier [(String, Type)] where
  pretty args = intercalate ", " $ map pretty args

instance Prettier (String, Type) where
  pretty (name, ty) = blue name ++ " : " ++ pretty ty

instance Prettier (Decl TypecheckedP) where
  pretty (VarDecl _ name ty expr) = pretty ty ++ ' ' : blue name ++ " = " ++ pretty expr ++ ";\n"
  pretty (FunDecl _ name retty args funvars body) =
    blue name
      ++ "("
      ++ pretty args
      ++ ") : "
      ++ pretty retty
      ++ " {\n"
      ++ (if not $ null funvars then indent $ pretty funvars else "")
      ++ pretty body
      ++ "}"
  prettyBrief (FunDecl _ name retty args funvars body) =
    '\n'
      : blue name
      ++ "("
      ++ prettyBrief args
      ++ ") : "
      ++ prettyBrief retty
      ++ " {\n"
      ++ (if not $ null funvars then indent $ prettyBrief funvars else "")
      ++ indent (prettyBrief body)
      ++ "\n}"
  prettyBrief var@VarDecl {} = pretty var

instance Prettier [Stmt p] where
  pretty = indent . foldMap (\x -> pretty x ++ "\n")
  prettyBrief = init . indentlittle . foldMap prettyBrief

instance Prettier (Stmt p) where
  pretty (ReturnStmt _ Nothing) = red "return" ++";"
  pretty (ReturnStmt _ (Just expr)) = red "return " ++ pretty expr ++ ";"
  pretty (IfStmt _ condition consequence Nothing) =
    red "if"
      ++ " ("
      ++ pretty condition
      ++ ") {\n"
      ++ pretty consequence
      ++ "}"
  pretty (IfStmt _ condition consequence (Just alternative)) =
    red "if"
      ++ " ("
      ++ pretty condition
      ++ ") {\n"
      ++ pretty consequence
      ++ "} " ++ red "else" ++ " {\n"
      ++ pretty alternative
      ++ "}"
  pretty (WhileStmt _ condition body) =
    red "while"
      ++ " ("
      ++ pretty condition
      ++ ") {\n"
      ++ pretty body
      ++ "}"
  pretty (ExprStmt _ expr) = pretty expr ++ ";"
  pretty (AssignStmt _ var expr) = pretty var ++ " = " ++ pretty expr ++ ";"
  pretty (BlockStmt statements) = "{" ++ pretty statements ++ "}"
  prettyBrief (ReturnStmt _ Nothing) = red "return" ++ ";"
  prettyBrief (ReturnStmt _ (Just expr)) = red "return " ++ prettyBrief expr ++ "; "
  prettyBrief (IfStmt _ condition consequence Nothing) =
    red "if" ++ " ("
      ++ prettyBrief condition
      ++ ") {"
      ++ prettyBrief consequence
      ++ "}\n"
  prettyBrief (IfStmt _ condition consequence (Just alternative)) =
    red "if" ++" ("
      ++ prettyBrief condition
      ++ ") {"
      ++ prettyBrief consequence
      ++ "} else { "
      ++ prettyBrief alternative
      ++ "}"
  prettyBrief (WhileStmt _ condition body) =
    red "while" ++" ("
      ++ prettyBrief condition
      ++ ") {"
      ++ prettyBrief body
      ++ "}\n"
  prettyBrief (ExprStmt _ expr) = prettyBrief expr ++ "; "
  prettyBrief (AssignStmt _ var expr) = pretty var ++ " = " ++ prettyBrief expr ++ "; "
  prettyBrief (BlockStmt statements) = "{" ++ prettyBrief statements ++ "} "

-- pretty (VarStmt _ (Just ty) identifier expr) = formatType ty ++ " " ++ identifier ++ " = " ++ formatExpr expr ++ ";"

formatField :: Field -> String
formatField HeadField = "hd"
formatField TailField = "tl"

indent :: String -> String
indent str = unlines $ map (replicate 4 ' ' ++) $ lines str

indentlittle :: String -> String
indentlittle str = unlines $ map (replicate 1 ' ' ++) $ lines str

instance Prettier (Expr p) where
  pretty (BinOpExpr _ op expr1 expr2) = pretty expr1 ++ formatBinOp op ++ pretty expr2
    where
      formatBinOp Mul = " * "
      formatBinOp Div = " / "
      formatBinOp Mod = " % "
      formatBinOp Add = " + "
      formatBinOp Sub = " - "
      formatBinOp Cons = ":"
      formatBinOp Gt = " > "
      formatBinOp Gte = " >= "
      formatBinOp Lt = " < "
      formatBinOp Lte = " <= "
      formatBinOp Eq = " == "
      formatBinOp Neq = " != "
      formatBinOp And = " && "
      formatBinOp Or = " || "
  pretty (UnaryOpExpr _ Negate expr) = "!" ++ pretty expr
  pretty (UnaryOpExpr _ (FieldAccess field) expr) = pretty expr ++ "." ++ pretty field
  pretty (FunctionCallExpr _ name args) = blue name ++ "(" ++ intercalate ", " (map pretty args) ++ ")"
  pretty (VariableExpr _ variable) = pretty variable
  pretty (LiteralExpr _ literal) = pretty literal

instance Prettier Field where
  pretty = show

instance Prettier Variable where
  pretty (Identifier name Nothing) = blue name
  pretty (Identifier name (Just field)) = blue name ++ '.' : pretty field

instance Prettier Type where
  pretty = show

instance Prettier (Literal p) where
  pretty TrueLit = black "True"
  pretty FalseLit = black "False"
  pretty (IntLit n) = black $ show n
  pretty (CharLit c) = black $ show c
  pretty (TupleLit (expr1, expr2)) = "(" ++ pretty expr1 ++ "," ++ pretty expr2 ++ ")"
  pretty EmptyListLit = black "[]"

{-
indent :: String -> String
indent str = unlines $ map (replicate 4 ' ' ++) $ lines str

formatProgram :: Program -> String
formatProgram = concatMap formatDecl

formatType :: Type -> String
formatType IntType = "Int"
formatType CharType = "Char"
formatType BoolType = "Bool"
formatType VoidType = "Void"
formatType (TupleType ty1 ty2) = "(" ++ formatType ty1 ++ ", " ++ formatType ty2 ++ ")"
formatType (ListType ty) = "[" ++ formatType ty ++ "]"
formatType (TypeVar var) = var

formatDecl :: Decl -> String
formatDecl (FunDecl name Nothing args body) =
    name ++ "(" ++ formatFuncArgs args  ++ ")" ++ " {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") body)
    ++ "\n}\n\n"
formatDecl (FunDecl name (Just ty) args body) =
    name ++ "(" ++ formatFuncArgs args ++ ") : " ++ formatType ty ++ " {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") body)
    ++ "\n}\n\n"

formatFuncArgs :: [(String, Maybe Type)] -> String
formatFuncArgs args = intercalate ", " $ map formatSingle args
    where formatSingle (name, Nothing) = name
          formatSingle (name, Just ty) = name ++ " : " ++ formatType ty

formatStmt :: Stmt -> String
formatStmt (ReturnStmt Nothing) = "return;"
formatStmt (ReturnStmt (Just expr)) = "return " ++ formatExpr expr ++ ";"
formatStmt (IfStmt condition consequence Nothing) =
    "if (" ++ formatExpr condition ++ ") {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") consequence)
    ++ "}"
formatStmt (IfStmt condition consequence (Just alternative)) =
    "if (" ++ formatExpr condition ++ ") {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") consequence)
    ++ "} else {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") alternative)
    ++ "}"
formatStmt (WhileStmt condition body) =
    "while (" ++ formatExpr condition ++ ") {\n"
        ++ indent (foldMap (\x -> formatStmt x ++ "\n") body)
    ++ "}"
formatStmt (ExprStmt expr) = formatExpr expr ++ ";"
formatStmt (VarStmt Nothing identifier expr) = "var " ++ identifier ++ " = " ++ formatExpr expr ++ ";"
formatStmt (VarStmt (Just ty) identifier expr) = formatType ty ++ " " ++ identifier ++ " = " ++ formatExpr expr ++ ";"

formatField :: Field -> String
formatField HeadField = "hd"
formatField TailField = "tl"

formatVariable :: Variable -> String
formatVariable (Identifier name Nothing) = name
formatVariable (Identifier name (Just field)) = name ++ "." ++ formatField field

formatLiteral :: Literal -> String
formatLiteral TrueLit = "true"
formatLiteral FalseLit = "false"
formatLiteral (IntLit n) = show n
formatLiteral (FloatLit n) = show n
formatLiteral (CharLit c) = show c
formatLiteral (TupleLit (expr1, expr2)) = "(" ++ formatExpr expr1 ++ "," ++ formatExpr expr2 ++ ")"
formatLiteral EmptyListLit = "[]"
-}