module Token
( Token (..)
, identKind
) where

-- If it's an identifier or keyword, we get the right token here
identKind :: String -> Token
identKind "int" = IntTok
identKind "string" = StringTok
identKind "void" = VoidTok
identKind "if" = IfTok
identKind "else" = ElseTok
identKind "while" = WhileTok
identKind "return" = ReturnTok
identKind "write" = WriteTok
identKind "writeln" = WriteLnTok
identKind "read" = ReadTok
identKind a = IdentTok a

data Token =
        -- Idents and lits
        IdentTok String | NumTok Int | StrLitTok String |
        -- Type specifiers
        IntTok | StringTok | VoidTok |
        -- Control flow
        IfTok | ElseTok | WhileTok | ReturnTok |
        -- Input/Output
        WriteTok | WriteLnTok | ReadTok |
        -- syntax structure
        SemicolTok | CommaTok |
        -- brackets and braces
        LBrackTok | RBrackTok | LBraceTok | RBraceTok |
        LParenTok | RParenTok |
        -- assignment
        AssnTok |
        -- comparisons
        LessTok | LessEqTok | EqTok | NEqTok | GrtrTok | GrtrEqTok |
        -- add & sub
        PlusTok | MinusTok |
        -- mult and pointer
        AstrTok | DivTok | ModTok | AmprTok |
        -- EOF
        EOFTok
        deriving (Show, Eq)
