-- takes a stream of tokens and builds it into a parse tree
module Parser
( parse
) where

import Token
import Node

parse :: [Token] -> Node

declarationList :: [Token] -> (Node, [Token])
declarationList ts@(t:_) = let (tree, ts'@(t':_)) = declaration ts in
    case t' of
        EOFToken -> (DecList [tree], ts')
        _ -> let (DecList decs, ts'') = declarationList ts' in
            (DecList (tree:decs), ts'')

declaration :: [Token] -> (Node, [Token])
declaration _:_:[] = error "Improper declaration"
declaration ts@(_:_:thd:_) =
    case thd of
        LParenTok -> fundec ts
        _ -> vardec ts

vardec   :: [Token] -> (Node, [Token])
fundec   :: [Token] -> (Node, [Token])
-- maybe params?
param    :: [Token] -> (Node, [Token])
compstmt :: [Token] -> (Node, [Token])

statement :: [Token] -> (Node, [Token])
statement ts@(t:ts') =
    case t of
        LBrackTok -> compstmt ts'  -- compound statement
        IfTok     -> ifstmt ts'    -- if statement
        WhileTok  -> whilestmt ts' -- while statement
        ReturnTok -> retstmt ts'   -- return statement
        WriteTok  -> writestmt ts' -- write statement
        _         -> exprstmt ts   -- expression statement

ifstmt    :: [Token] -> (Node, [Token])
whilestmt :: [Token] -> (Node, [Token])
retstmt   :: [Token] -> (Node, [Token])
writestmt :: [Token] -> (Node, [Token])
exprstmt  :: [Token] -> (Node, [Token])

expression :: [Token] -> (Node, [Token])
compexp   :: [Token] -> (Node, [Token])
enode     :: [Token] -> (Node, [Token])
tnode     :: [Token] -> (Node, [Token])
fnode     :: [Token] -> (Node, [Token])
factor    :: [Token] -> (Node, [Token])
funcall   :: [Token] -> (Node, [Token])
-- maybe args?

expect :: Token -> [Token] -> [Token] -- confirm and eliminate extra stuff
expect _ [] = error "Found no tokens"
expect target ts
    | target == (head ts) = (tail ts)
    | otherwise = error "Expected token not found"

printTree :: Node -> String
printTree = printRec 0
