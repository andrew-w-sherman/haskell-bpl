-- input/output routines, calls to the components

import System.IO
import Scanner

main = do
    args <- getArgs
    contents <- readFile $ fst args
    tokens = Tokenize contents
    putStr tokens
