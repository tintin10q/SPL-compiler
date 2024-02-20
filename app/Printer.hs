module Printer where
import Parser.AST

formatProgram :: Program -> String
formatProgram = concatMap formatDecl

formatDecl :: Decl -> String
formatDecl (FunDecl name Nothing args body) = name ++ "(" ++ formatFuncArgs args  ++ ")" ++ "\n{\n" ++ foldMap formatStmt body ++ "\n}" 
formatDecl (FunDecl name (Just ty) args body) = name ++ "(" ++ formatFuncArgs args ++ ") :" ++ formatType ty ++ "\n{\n" ++ foldMap formatStmt body ++"\n}"

formatFuncArgs :: [(String, Maybe Type)] -> String
formatFuncArgs [] = ""
formatFuncArgs a = foldMap ( ("," ++) . formatFuncArg) a

formatFuncArg :: (String, Maybe Type) -> String
formatFuncArg (str, Nothing) = str 
formatFuncArg (str, Just ty) = str ++ " : " ++ formatType ty


formatStmt :: Stmt -> String
formatStmt (ReturnStmt Nothing) = "return;\n"
formatStmt (ReturnStmt (Just expr)) = "return " ++ formatExpr expr ++ ";\n"
formatStmt (IfStmt condition consequence Nothing) = "if (" ++ formatExpr condition ++ ")\n{\n" ++ foldMap formatStmt consequence ++ "\n}\n"
formatStmt (IfStmt cond consequence (Just statements)) = formatStmt (IfStmt cond consequence Nothing) ++ "else {\n" ++ foldMap formatStmt statements ++ "\n}\n"
formatStmt (WhileStmt condition body) = "while (" ++ formatExpr condition ++ ")\n{\n" ++ foldMap formatStmt body ++ "\n}\n" 
formatStmt (ExprStmt expr) = formatExpr expr ++ ";\n"
formatStmt (VarStmt Nothing identifier expr) = "var " ++ identifier ++ " = " ++ formatExpr expr ++ ";\n"
formatStmt (VarStmt (Just ty) identifier expr) = formatType ty ++ " " ++ identifier ++ " = " ++ formatExpr expr ++ ";\n"

formatExpr :: Expr -> String
formatExpr (BinOp op expr1 expr2) = formatExpr expr1 ++ formatBinOp op ++ formatExpr expr2
formatExpr (UnaryOp op expr) = formatUnaryOp op ++ formatExpr expr
formatExpr (AssignExpr variable expr) = formatVariable variable ++ formatExpr expr
formatExpr (FunctionCall name args) = name ++ "(" ++ foldMap (( "," ++ ) . formatExpr) args  ++ ")"
formatExpr (VariableExpr variable) = formatVariable variable
formatExpr (LiteralExpr literal) = formatLiteral literal

formatType :: Type -> String
formatType IntType = "Int"             -- Int
formatType CharType  = "Char"          -- Char
formatType BoolType  = "Bool"          -- Bool
formatType VoidType   = "Void"         -- Void
formatType (TupleType t1 t2) = "(" ++ formatType t1 ++ ", " ++ formatType t2 ++ ")" -- (a, b)
formatType (ListType ty) = "[" ++ formatType ty ++ "]"       -- [a]
formatType (TypeVar ty) = ty      -- a


formatVariable :: Variable -> String
formatVariable (Identifier name Nothing) = name
formatVariable (Identifier name (Just field)) = name ++ formatField field 

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


formatUnaryOp :: UnaryOp -> String 
formatUnaryOp Negate = "!"
formatUnaryOp (FieldAccess field) = "." ++ formatField field 

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