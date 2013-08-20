module Keen.Main where

import Keen.Syntax
import Keen.Parser

main = do
    f <- readFile "test.txt"
    case parse f of
        Right r -> print r
        Left e -> print e


-- End of file
-- nice due to a bug in this IDE
-- that hides the last lines
