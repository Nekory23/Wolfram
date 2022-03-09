module Rules where

getFirstLine :: Int -> Int -> String -> String
getFirstLine rule 0 line = line
getFirstLine rule start line = 
    getFirstLine rule (start - 1) (generate rule line)

generate :: Int -> [Char] -> String
generate 30 line = rule30 line "  "
generate 90 line = rule90 line "  "
generate 110 line = rule110 line "  "

rule30 :: [Char] -> [Char] -> [Char]
rule30 ('*':'*':'*':rest) line = rule30 ('*':'*':rest) (line ++ " ")
rule30 ('*':'*':' ':rest) line = rule30 ('*':' ':rest) (line ++ " ")
rule30 ('*':' ':'*':rest) line = rule30 (' ':'*':rest) (line ++ " ")
rule30 ('*':' ':' ':rest) line = rule30 (' ':' ':rest) (line ++ "*")
rule30 (' ':'*':'*':rest) line = rule30 ('*':'*':rest) (line ++ "*")
rule30 (' ':'*':' ':rest) line = rule30 ('*':' ':rest) (line ++ "*")
rule30 (' ':' ':'*':rest) line = rule30 (' ':'*':rest) (line ++ "*")
rule30 (' ':' ':' ':rest) line = rule30 (' ':' ':rest) (line ++ " ")
rule30 _ line = line ++ "  "

rule90 :: [Char] -> [Char] -> [Char]
rule90 ('*':'*':'*':rest) line = rule90 ('*':'*':rest) (line ++ " ")
rule90 ('*':'*':' ':rest) line = rule90 ('*':' ':rest) (line ++ "*")
rule90 ('*':' ':'*':rest) line = rule90 (' ':'*':rest) (line ++ " ")
rule90 ('*':' ':' ':rest) line = rule90 (' ':' ':rest) (line ++ "*")
rule90 (' ':'*':'*':rest) line = rule90 ('*':'*':rest) (line ++ "*")
rule90 (' ':'*':' ':rest) line = rule90 ('*':' ':rest) (line ++ " ")
rule90 (' ':' ':'*':rest) line = rule90 (' ':'*':rest) (line ++ "*")
rule90 (' ':' ':' ':rest) line = rule90 (' ':' ':rest) (line ++ " ")
rule90 _ line = line ++ "  "

rule110 :: [Char] -> [Char] -> [Char]
rule110 ('*':'*':'*':rest) line = rule110 ('*':'*':rest) (line ++ " ")
rule110 ('*':'*':' ':rest) line = rule110 ('*':' ':rest) (line ++ "*")
rule110 ('*':' ':'*':rest) line = rule110 (' ':'*':rest) (line ++ "*")
rule110 ('*':' ':' ':rest) line = rule110 (' ':' ':rest) (line ++ " ")
rule110 (' ':'*':'*':rest) line = rule110 ('*':'*':rest) (line ++ "*")
rule110 (' ':'*':' ':rest) line = rule110 ('*':' ':rest) (line ++ "*")
rule110 (' ':' ':'*':rest) line = rule110 (' ':'*':rest) (line ++ "*")
rule110 (' ':' ':' ':rest) line = rule110 (' ':' ':rest) (line ++ " ")
rule110 _ line = line ++ "  "