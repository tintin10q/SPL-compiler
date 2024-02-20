module Printer where
import Parser.AST

formatProgram :: Program -> String
formatProgram = concatMap formatDecl

formatDecl :: Decl -> String
formatDecl (FunDecl name ty args body) = ""

formatStmt :: Stmt -> String
formatStmt (ReturnStmt expr) = ""
formatStmt (IfStmt condition consequence alternative) = ""
formatStmt (WhileStmt condition body) = ""
formatStmt (ExprStmt expr) = ""
formatStmt (VarStmt ty identifier expr) = ""

formatExpr :: Expr -> String
formatExpr (BinOp op expr1 expr2) = ""
formatExpr (UnaryOp op expr) = ""
formatExpr (AssignExpr variable expr) = ""
formatExpr (FunctionCall name args) = ""
formatExpr (VariableExpr variable) = ""
formatExpr (LiteralExpr literal) = formatLiteral literal

formatLiteral :: Literal -> String
formatLiteral TrueLit = "true"
formatLiteral FalseLit = "false"
formatLiteral (IntLit n) = show n
formatLiteral (FloatLit n) = show n
formatLiteral (CharLit c) = show c
formatLiteral (TupleLit (expr1, expr2)) = "(" ++ formatExpr expr1 ++ "," ++ formatExpr expr2 ++ ")"
formatLiteral EmptyListLit = "[]"