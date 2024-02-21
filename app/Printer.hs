module Printer where
import Parser.AST
import Data.List (intercalate)

tabSize :: Int
tabSize = 4

indent :: String -> String
indent str = unlines $ map (replicate tabSize ' ' ++) $ lines str

formatProgram :: Program -> String
formatProgram = concatMap formatDecl

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

formatExpr :: Expr -> String
formatExpr (BinOp op expr1 expr2) = formatExpr expr1 ++ formatBinOp op ++ formatExpr expr2
formatExpr (UnaryOp Negate expr) = "!" ++ formatExpr expr
formatExpr (UnaryOp (FieldAccess field) expr) = formatExpr expr ++ "." ++ formatField field
formatExpr (AssignExpr variable expr) = formatVariable variable ++ " = " ++ formatExpr expr
formatExpr (FunctionCall name args) = name ++ "(" ++ intercalate ", " (map formatExpr args) ++ ")"
formatExpr (VariableExpr variable) = formatVariable variable
formatExpr (LiteralExpr literal) = formatLiteral literal

formatType :: Type -> String
formatType IntType = "Int"
formatType CharType  = "Char"
formatType BoolType  = "Bool"
formatType VoidType   = "Void"
formatType (TupleType t1 t2) = "(" ++ formatType t1 ++ ", " ++ formatType t2 ++ ")"
formatType (ListType ty) = "[" ++ formatType ty ++ "]"
formatType (TypeVar ty) = ty

formatVariable :: Variable -> String
formatVariable (Identifier name Nothing) = name
formatVariable (Identifier name (Just field)) = name ++ "." ++ formatField field

formatBinOp :: BinOp -> String
formatBinOp Mul = " * "
formatBinOp Div = " / "
formatBinOp Mod = " % "
formatBinOp Add = " + "
formatBinOp Sub = " - "
formatBinOp Cons= ":"
formatBinOp Gt  = " > "
formatBinOp Gte = " >= "
formatBinOp Lt  = " < "
formatBinOp Lte = " <= "
formatBinOp Eq  = " == "
formatBinOp Neq = " != "
formatBinOp And = " && "
formatBinOp Or  = " || "

formatField :: Field -> String
formatField HeadField = "hd"
formatField TailField = "tl"

formatLiteral :: Literal -> String
formatLiteral TrueLit = "true"
formatLiteral FalseLit = "false"
formatLiteral (IntLit n) = show n
formatLiteral (FloatLit n) = show n
formatLiteral (CharLit c) = show c
formatLiteral (TupleLit (expr1, expr2)) = "(" ++ formatExpr expr1 ++ "," ++ formatExpr expr2 ++ ")"
formatLiteral EmptyListLit = "[]"