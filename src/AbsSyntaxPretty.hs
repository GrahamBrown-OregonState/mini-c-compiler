--
-- Mini-C abstract syntax with pretty printing
--
module AbsSyntaxPretty where

-- Program in mini-C: list of declarations.
--
newtype Program = Program [Decl]

-- Declaration:
-- Variable declaration: type, variable name, [array dimension].
-- Function declaration: type, function name, parameter list, function body.
--
data Decl = VarDecl Type String (Maybe Int)
          | FunDecl Type String [Param] Comp
          deriving (Eq)

-- Data types in mini-C: int & void.
--
data Type = Int | Void
          deriving (Eq)

-- Parameter: type, name, single(True) or array(False).
--
data Param = Param Type String Bool
           deriving (Eq)

-- Compound statement: local declarations and statements.
--
data Comp = Comp [Decl] [Stmt]
          deriving (Eq)

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
          deriving (Eq)

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
          deriving (Eq)

-- Variable: name, [array index].
--
data Var = Var String (Maybe Expr)
         deriving (Eq)

-- Operators: <=, <, >, >=, ==, !=, +, -, *, /
--
data Op = OpLEQ | OpLT | OpGT | OpGEQ | OpEQ | OpNEQ
        | OpPlus | OpMinus | OpTimes | OpDiv
        deriving (Eq)


-- The following part defines a pretty printer for Mini-C programs.
-- 
class PrettyPrint a where
  pp :: Int -> a -> String

indent :: Int
indent = 4

instance Show Program where
  show (Program []) = ""
  show (Program decls) = foldl1 ((++).(++"\n")) (map (pp 0) decls)

instance Show Decl where
  show = pp 0

instance PrettyPrint Decl where
  pp n (VarDecl t v Nothing) = replicate n ' '++show t++" "++v++";"
  pp n (VarDecl t v (Just i)) = replicate n ' '++show t++" "++v++"["++show i++"];"
  pp n (FunDecl t f [] s) = replicate n ' '++show t++" "++f++" (void)\n"++(pp n s)
  pp n (FunDecl t f params s) = replicate n ' '++show t++" "++f++" ("
    ++(foldl1 ((++).(++", ")) (map show params))++")\n"++(pp n s)

instance Show Type where
  show Int = "int"
  show Void = "void"

instance Show Param where
  show (Param t v True) = show t++" "++v
  show (Param t v False) = show t++" "++v++"[]"

instance Show Comp where
  show = pp 0

instance PrettyPrint Comp where
  pp n (Comp decls stmts) = replicate n ' '++"{"
    ++concatMap (("\n"++).(pp (n+indent))) decls
    ++concatMap (("\n"++).(pp n)) stmts
    ++"\n"++(replicate n ' ')++"}"

instance Show Stmt where
  show = pp 0

instance PrettyPrint Stmt where
  pp n (ExprStmt Nothing) = replicate (n+indent) ' '++";"
  pp n (ExprStmt (Just e)) = replicate (n+indent) ' '++show e++";"
  pp n (CompStmt s) = pp n s
  pp n (SelStmt e s1 Nothing) = replicate (n+indent) ' '++"if ("++show e++")\n"++pp (n+indent) s1
  pp n (SelStmt e s1 (Just s2)) = replicate (n+indent) ' '++"if ("++show e++")\n"
    ++pp (n+indent) s1++"\n"++replicate (n+indent) ' '++"else\n"++pp (n+indent) s2
  pp n (IterStmt e s) = replicate (n+indent) ' '++"while ("++show e++")\n"++pp (n+indent) s
  pp n (RetStmt Nothing) = replicate (n+indent) ' '++"return;"
  pp n (RetStmt (Just e)) = replicate (n+indent) ' '++"return "++show e++";"

instance Show Expr where
  show (AssignExpr s e) = show s ++ " = " ++ show e
  show (OpExpr o e1 e2) = "(" ++ show e1 ++ show o ++ show e2 ++ ")"
  show (VarExpr v) = show v
  show (CallExpr f []) = f ++ " ()"
  show (CallExpr f args) = f++" ("++(foldl1 ((++).(++", ")) (map show args))++")"
  show (NumExpr i) = show i

instance Show Var where
  show (Var s Nothing) = s
  show (Var s (Just i)) = s++"["++show i++"]"

instance Show Op where
  show OpLEQ   = "<="
  show OpLT    = "<"
  show OpGT    = ">"
  show OpGEQ   = ">="
  show OpEQ    = "=="
  show OpNEQ   = "!="
  show OpPlus  = "+"
  show OpMinus = "-"
  show OpTimes = "*"
  show OpDiv   = "/"
