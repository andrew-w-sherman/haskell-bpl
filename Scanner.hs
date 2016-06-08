-- takes a file with bpl stuff and tokenizies it
module Scanner
( tokenize
) where

import Token
import Data.Char

tokenize :: String -> [Token]
tokenize [] = [EOFTok]
tokenize (c:cs)
    -- in neutral state, whitespace is ignored
    | isWS c = tokenize cs
    -- detecting alpha from neutral, consume alphanums
    | isAlpha c = identifier c cs
    -- detecting num from neutral, consume nums
    | isDigit c = number c cs
    -- detecting string literals from neutral
    | isDoubleQuote c = strlit c cs
    -- forward slash is weird and gets its own case
    | isFwSlash c = fwSlash c cs
    -- other punctuation
    | isRelevantPunct c = operator c cs
    -- otherwise
    | otherwise = error $ "Cannot tokenize " ++ [c]


-- consume alphanum
identifier :: Char -> String -> [Token]
identifier c cs = let (str, cs') = span isAlphaNum cs in
                  identKind (c:str) : tokenize cs'

-- consume numbers
number :: Char -> String -> [Token]
number c cs = let (str, cs') = span isDigit cs in
                  NumTok (read (c:str)) : tokenize cs'

-- consume everything that isn't a quote
strlit :: Char -> String -> [Token]
strlit c cs = let (str, cs') = break isDoubleQuote cs in
                  StrLitTok (str) : tokenize (tail cs')

-- forward slash case
fwSlash :: Char -> String -> [Token]
fwSlash c cs
    | (head cs) == '*' = comment (tail cs)
    | otherwise = operator c cs

-- throw away anything in comment tags
comment :: String -> [Token]
comment [] = [EOFTok]
comment ('*':'/':cs) = tokenize cs
comment (c:cs) = comment cs

-- deal with operators!!!
operator :: Char -> String -> [Token]
operator c [] = operator c [' '] -- special case for eof, add whitespace
operator c cs@(c':cs')
    | c == ';' = SemicolTok : tokenize cs
    | c == ',' = CommaTok : tokenize cs
    | c == '[' = LBrackTok : tokenize cs
    | c == ']' = RBrackTok : tokenize cs
    | c == '{' = LBraceTok : tokenize cs
    | c == '}' = RBraceTok : tokenize cs
    | c == '(' = LParenTok : tokenize cs
    | c == ')' = RParenTok : tokenize cs
    | c == '<' && c' == '=' = LessEqTok : tokenize cs'
    | c == '<' = LessTok : tokenize cs
    | c == '>' && c' == '=' = GrtrEqTok : tokenize cs'
    | c == '>' = GrtrTok : tokenize cs
    | c == '=' && c' == '=' = EqTok : tokenize cs'
    | c == '=' = AssnTok : tokenize cs
    | c == '!' && c' == '=' = NEqTok : tokenize cs'
    | c == '+' = PlusTok : tokenize cs
    | c == '-' = MinusTok : tokenize cs
    | c == '*' = AstrTok : tokenize cs
    | c == '/' = DivTok : tokenize cs
    | c == '%' = ModTok : tokenize cs
    | c == '&' = AmprTok : tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

-- some custom predicates
isDoubleQuote :: Char -> Bool
isDoubleQuote c = c == '\"'

isFwSlash :: Char -> Bool
isFwSlash c = c == '/'

isWS :: Char -> Bool
isWS c = elem c " \n\t"

isRelevantPunct :: Char -> Bool
isRelevantPunct = flip elem ";,[](){}=<!>+-*%*"
