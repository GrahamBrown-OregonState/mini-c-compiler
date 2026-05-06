{
module Happy.Parser (parse) where

import AbsSyntax
import Scanner (scan, Token(..))

happyError :: [Token] -> a
happyError _ = error "Parse error"

parse :: String -> Program
parse = parser . scan

parseF :: String -> IO ()
parseF f = do s <- readFile f
    putStr (show (parse s))
}

%name parser

%tokentype { Token }
%token
    else   { TElse }
    if     { TIf }
    int    { TInt }
    return { TReturn }
    void   { TVoid }
    while  { TWhile }
    '+'    { TPlus }
    '-'    { TMinus }
    '*'    { TMult }
    '/'    { TDiv }
    '<='   { TLessThanEq }
    '<'    { TLessThan }
    '>='   { TGreaterThanEq }
    '>'    { TGreaterThan }
    '=='   { TEqEq }
    '!='   { TNotEq }
    '='    { TEq }
    ';'    { TSemiCol }
    ','    { TComma }
    '('    { TLParen }
    ')'    { TRParen }
    '['    { TLeftBrack }
    ']'    { TRightBrack }
    '{'    { TLeftCurly }
    '}'    { TRightCurly }
    id     { TId $$ }
    num    { TNum $$ }


%%
-- TODO: Finish productions!
Program : [Decl]    {Program $1}

Decl : Type id ';'                      {VarDecl $1 $2 Nothing}
     | Type id '[' num ']' ';'          {VarDecl $1 $2 Just $4}
     | Type String '(' [Param] ')' Comp {FunDecl $1 $2 $3 $4}

Type : int  { Int }
     | void { Void }

Param : Type id         {Param $1 $2 True }
      | Type id '[' ']' { Param $1 $2 False}

Comp : '{' [Decl] [Stmt] '}'   {Comp $2 $3}

-- COMPLETE
Stmt : ';'                              { ExprStmt Nothing }
     | Expr ';'                         { ExprStmt Just $1}
     | Comp                             { CompStmt $1 }
     | if '(' Expr ')' Stmt             { SelStmt $3 $5 Nothing }
     | if '(' Expr ')' Stmt else Stmt   { SelStmt $3 $5 Just $7}
     | while '(' Expr ')' Stmt          { IterStmt $3 $5 }
     | return ';'                       { RetStmt Nothing }  
     | return Expr ';'                  { RetStmt Just $2 }

Expr : Var '=' Expr         { AssignExpr $1 $3}
     | Expr Op Expr         { OpExpr $2 $1 $3}
     | Var                  { VarExpr $1 }
     | id '(' [Expr] ')'    { CallExpr $1 $3}
     | int                  { NumExpr $1 }

Var : id                {Var $1 Nothing}
    | id '[' Expr ']'   {Var $1 Just $2}

Op : '<=' { OpLeq }
   | '<'  { OpLT }
   | '>'  { OpGT }
   | '>=' { OpGEQ }
   | '==' { OpEQ }
   | '!=' { OpNEQ }
   | '+'  { OpPlus }
   | '-'  { OpMinus }
   | '*'  { OpTimes }
   | '/'  { OpDiv }
