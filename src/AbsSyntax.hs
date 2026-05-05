--
-- Mini-C abstract syntax
--
module AbsSyntax where

-- Program in mini-C: list of declarations.
--
newtype Program = Program [Decl]
                  deriving Show

-- Declaration:
-- Variable declaration: type, variable name, [array dimension].
-- Function declaration: type, function name, parameter list, function body.
--
data Decl = VarDecl Type String (Maybe Int)
          | FunDecl Type String [Param] Comp
          deriving (Eq,Show)

-- Data types in mini-C: int & void.
--
data Type = Int | Void
          deriving (Eq,Show)

-- Parameter: type, name, single(True) or array(False).
--
data Param = Param Type String Bool
           deriving (Eq,Show)

-- Compound statement: local declarations and statements.
--
data Comp = Comp [Decl] [Stmt]
          deriving (Eq,Show)

-- Statement:
-- Expression statement: [expression].
-- Compound statement: compound statement.
-- Selective statement: conditional expression, if branch, [else branch].
-- Return statement: [expression].
--
data Stmt = ExprStmt (Maybe Expr)
          | CompStmt Comp
          | SelStmt Expr Stmt (Maybe Stmt) 
          | IterStmt Expr Stmt
          | RetStmt (Maybe Expr)
          deriving (Eq,Show)

-- Expression:
-- Assignment expression: variable name, expression.
-- Operation expression: operator, expression, expression.
-- Variable expression: variable.
-- Call expression: function name, arguments.
-- Numeric expression: number.
--
data Expr = AssignExpr Var Expr 
          | OpExpr Op Expr Expr
          | VarExpr Var
          | CallExpr String [Expr]
          | NumExpr Int
          deriving (Eq,Show)

-- Variable: name, [array index].
--
data Var = Var String (Maybe Expr)
         deriving (Eq,Show)

-- Operators: <=, <, >, >=, ==, !=, +, -, *, /
--
data Op = OpLEQ | OpLT | OpGT | OpGEQ | OpEQ | OpNEQ
        | OpPlus | OpMinus | OpTimes | OpDiv
        deriving (Eq,Show)

