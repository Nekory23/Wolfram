module GestArgs where

import Data.Char

checkValue :: String -> Bool 
checkValue [current] | isDigit current = True
                     | otherwise = False
checkValue (current:rest) | isDigit current = checkValue rest
                          | otherwise = False
checkValue _ = False

checkValueNeg :: String -> Bool 
checkValueNeg [current] = checkValue [current]
checkValueNeg ('-':rest) = checkValue rest
checkValueNeg value = checkValue value

changeValue :: [a] -> (Int, a) -> [a]
changeValue [] _ = []
changeValue (_:rest) (0, a) = a:rest
changeValue (start:rest) (n, a) 
    | n < 0 = start:rest
    | otherwise = start:changeValue rest (n - 1, a)

getOptions :: [String] -> [String] -> [String]
getOptions ("--rule":v:rest) opt
    | checkValue v = getOptions rest (changeValue opt (0, v))
    | otherwise = getOptions rest (changeValue opt (0, "err"))
getOptions ("--start":v:rest) opt
    | checkValue v = getOptions rest (changeValue opt (1, v))
    | otherwise = getOptions rest (changeValue opt (1, "err"))
getOptions ("--lines":v:rest) opt
    | checkValue v = getOptions rest (changeValue opt (2, v))
    | otherwise = getOptions rest (changeValue opt (2, "err"))
getOptions ("--window":v:rest) opt
    | checkValue v = getOptions rest (changeValue opt (3, v))
    | otherwise = getOptions rest (changeValue opt (3, "err"))
getOptions ("--move":v:rest) opt
    | checkValueNeg v = getOptions rest (changeValue opt (4, v))
    | otherwise = getOptions rest (changeValue opt (4, "err"))
getOptions (_:v:rest) opt = ["null"]
getOptions _ opt = opt