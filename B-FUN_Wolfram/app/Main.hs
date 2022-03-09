import System.Environment
import System.IO
import System.Exit
import Data.List
import Text.Read

import GestArgs
import DisplayLines
import Rules
import Values

displayError :: String -> IO a
displayError error = hPutStrLn stderr usage
                        >> hPutStrLn stderr ("Error: " ++ error)
                        >> exitWith (ExitFailure 84)

checkErrors :: [String] -> IO ()
checkErrors [] = displayError "This program must take at least 2 arguments"
checkErrors [a] = displayError "The first argument has no value"
checkErrors args 
    | odd (length args) = displayError "The number of arguments must be even"
    | "--rule" `notElem` args = displayError "There is no --rule setting"
    | otherwise = return ()

checkOptions :: [String] -> IO ()
checkOptions [] = return ()
checkOptions ["null"] = displayError "One of the value is not valid"
checkOptions ["err"] = displayError "One of the value is not valid"
checkOptions ("null":rest) = displayError "One of the value is not valid"
checkOptions ("err":rest) = displayError "One of the value is not valid"
checkOptions (_:rest) = checkOptions rest

checkSupported :: Int -> IO ()
checkSupported 30 = return ()
checkSupported 90 = return ()
checkSupported 110 = return ()
checkSupported rule
    | rule > 255 = displayError "This rule does not exist"
    | rule < 0 = displayError "This rule does not exist"
checkSupported _ = putStrLn "This rule is not supported" >> exitSuccess

main :: IO ()
main = do
    args <- getArgs
    checkErrors args
    let optStr = getOptions args defaultOpt
    checkOptions optStr
    let optNbr = map (read::String -> Int) optStr
    checkSupported (head optNbr)
    let firstLine = getFirstLine (head optNbr) (optNbr !! 1) "  *  "
    displayGeneration optNbr (head optNbr) (optNbr !! 2) firstLine
    exitSuccess