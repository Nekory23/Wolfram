module DisplayLines where

import Rules

resizeLine :: [Char] -> Int -> [Char]
resizeLine line 0 = line
resizeLine line off
    | off < 0 = resizeLine (tail line) (off + 1)
    | otherwise = resizeLine (" " ++ line) (off - 1)

cutBegin :: [Char] -> Int -> [Char]
cutBegin line 0 = line
cutBegin line move = cutBegin (init line) (move - 1)

addMove :: [Char] -> Int -> [Char]
addMove line 0 = line
addMove line move = addMove (line ++ " ") (move - 1)

resizeMove :: [Char] -> Int -> [Char]
resizeMove line 0 = line
resizeMove line move 
    | move < 0 = reverse (cutBegin (reverse line) (-move - 1))
    | move > 0 = reverse (addMove (reverse line) (move - 1))

printLine :: [Char] -> Int -> Int -> IO ()
printLine line window move =
    let offset = window `div` 2 - length line `div` 2
        resizedLine = resizeLine (reverse (resizeLine (reverse line) offset))
        resizedMove = resizeMove (resizedLine offset) move 
    in putStrLn (init resizedMove)

displayGeneration :: [Int] -> Int -> Int -> [Char] -> IO ()
displayGeneration options rule 0 line = return ()
displayGeneration options rule count line 
    | count < 1 = printLine line (options !! 3) (options !! 4) >>
                  displayGeneration options rule count (generate rule line)
    | otherwise = printLine line (options !! 3) (options !! 4) >>
                  displayGeneration options rule (count-1) (generate rule line)