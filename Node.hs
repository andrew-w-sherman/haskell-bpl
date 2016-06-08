-- nodes for the parse tree
module Node
( Node(..)
, children
, printRec
) where

import Token

data Node = 
    -- Declarations
    DecList [Node] | VarDec Token Token | PtrDec Token Token |
    ArrDec Token Token Token | FunDec Token Token [Node] Node |
    Param Token Token | ParamArr Token Token | ParamPtr Token Token |
    -- Statements
    CompStmt [Node] [Node] | IfStmt Node Node | IfElse Node Node Node |
    WhileStmt Node Node | RetStmt | RetValueStmt Node | WriteStmt Node |
    WriteLnStmt
    -- Expressions
    AssnExp Node Node | CompExp Node Token Node |
    ENode Node Token Node | TNode Node Token Node | FNeg Node |
    FPtr Token Node | ReadExp | FunCall Token [Node] | Lit Token |
    VarPtr Token | VarArr Token Node | Var Token
    deriving (Show, Eq)

children :: Node -> [Node]
children DecList decs = decs
children FunDec params cs = params ++ cs
children CompStmt lds vds = lds ++ vds
children IfStmt ex st = [ex, st]
children IfElse ex st1 st2 = [ex, st1, st2]
children WhileStmt ex st = [ex, st]
children RetValueStmt ex = [ex]
children WriteStmt ex = [ex]
children AssnExp var ex = [var, ex]
children CompExp e1 e2 = [e1, e2]
children ENode e t = [e, t]
children TNode t f = [t, f]
children FNeg f = [f]
children FPtr fact = [fact]
children FunCall args = args
children VarArr ex = [ex]
children _ = []

printRec :: Int -> Node -> String
printRec depth tree =
    let prefix = repeat depth '\t' in
        prefix ++ (show tree) ++ '\n' ++
            concatMap (printRec (depth + 1)) (children tree)
