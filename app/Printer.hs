module Printer where
import Parser.AST

replicate_by = 4

formatProgram :: Program -> String
formatProgram = concatMap formatDecl

formatDecl :: Decl -> String

formatDecl (FunDecl name Nothing [] body) = name ++ "()" ++ "\n{\n" ++ foldMap (formatStmt 1) body ++ "\n}\n\n" 
formatDecl (FunDecl name (Just ty) [] body) = name ++ "() :" ++ formatType ty ++ "\n{\n" ++ foldMap (formatStmt 1) body ++"\n}\n\n"
formatDecl (FunDecl name Nothing (first:rest) body) = name ++ "(" ++ formatFuncArg first ++ formatFuncArgs rest  ++ ")" ++ "\n{\n" ++ foldMap (formatStmt 1) body ++ "\n}\n\n" 
formatDecl (FunDecl name (Just ty) (first:rest) body) = name ++ "(" ++ formatFuncArg first ++ formatFuncArgs rest ++ ") : " ++ formatType ty ++ "\n{\n" ++ foldMap (formatStmt 1) body ++"\n}\n\n"

formatFuncArg :: (String, Maybe Type) -> String 
formatFuncArg (str, Nothing) = str 
formatFuncArg (str, Just ty) = str ++ " : " ++ formatType ty

-- Adds comma before
formatFuncArgs :: [(String, Maybe Type)] -> String
formatFuncArgs [] = ""
formatFuncArgs (first:rest) = formatFuncArg first ++ foldMap ( ("," ++) . formatFuncArg) rest


makeIndent :: Int -> String
makeIndent indent = replicate (indent * replicate_by) ' '

formatStmt :: Int -> Stmt -> String
formatStmt indent (ReturnStmt Nothing) = makeIndent indent ++ "return;"
formatStmt indent (ReturnStmt (Just expr)) = makeIndent indent ++ "return " ++ formatExpr expr ++ ";"
formatStmt indent (IfStmt condition consequence Nothing) = makeIndent indent ++ "if (" ++ formatExpr condition ++ ")\n" ++ makeIndent indent ++ "{\n" ++ foldMap (formatStmt $ indent + 1) consequence ++ "\n"++ makeIndent indent ++"}\n\n"
formatStmt indent (IfStmt cond consequence (Just statements)) = formatStmt indent (IfStmt cond consequence Nothing) ++ "else {\n" ++ foldMap (formatStmt $ indent+1) statements ++ "\n}\n\n"
formatStmt indent (WhileStmt condition body) = makeIndent indent ++ "while (" ++ formatExpr condition ++ ")\n{\n" ++ foldMap (formatStmt indent) body ++ "\n}\n" 
formatStmt indent (ExprStmt expr) = makeIndent indent ++ formatExpr expr ++ ";\n"
formatStmt indent (VarStmt Nothing identifier expr) = makeIndent indent ++ "var " ++ identifier ++ " = " ++ formatExpr expr ++ ";\n"
formatStmt indent (VarStmt (Just ty) identifier expr) = makeIndent indent ++ formatType ty ++ " " ++ identifier ++ " = " ++ formatExpr expr ++ ";\n"

formatExpr :: Expr -> String
formatExpr (BinOp op expr1 expr2) = formatExpr expr1 ++ formatBinOp op ++ formatExpr expr2
formatExpr (UnaryOp op expr) = formatUnaryOp op ++ formatExpr expr
formatExpr (AssignExpr variable expr) = formatVariable variable ++ formatExpr expr
formatExpr (FunctionCall name []) = name ++ "()"
formatExpr (FunctionCall name (first:rest)) = name ++ "(" ++ formatExpr first ++ foldMap ((","++)  .formatExpr) rest ++ ")"
formatExpr (VariableExpr variable) = formatVariable variable
formatExpr (LiteralExpr literal) = formatLiteral literal

formatType :: Type -> String
formatType IntType = "Int"             -- Int
formatType CharType  = "Char"          -- Char/
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