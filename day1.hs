import Data.Char (digitToInt)
import Data.List (isPrefixOf)

firstAndLast :: String -> String
firstAndLast input
    | null input = ""
    | otherwise = [head input, last input]

nums :: String -> String
nums [] = []
nums (l:ls)
    | l >= '0' && l <= '9' = l : nums ls
    | otherwise = nums ls

main = do
    let filename = "day1.txt"
    content <- readFile(filename)
    let contentLines = lines content
    let onlyNumbers = map nums contentLines
    let correctNums = map firstAndLast onlyNumbers
    let total = sum (map read correctNums)
    putStrLn $ show total

replacePairsInAllStrings :: Eq a => [(a, a)] -> [[a]] -> [a]
replacePairsInAllStrings reps words = map replacePairs reps words

replacePairs :: Eq a => [(a, a)] -> [a] -> [a]
replacePairs [] str = str
replacePairs ((needle, replacement):pairs) str = replacePairs pairs (replace needle replacement str)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace needle replacement str
    | needle `isPrefixOf` str = replacement ++ replace needle replacement (drop (length needle) str)
    | otherwise = head str : replace needle replacement (tail str)


-- oneReplace :: (String, String) -> String -> String
-- oneReplace [] String = String
-- oneReplace (needle, replace) haystack = 

part2 = do
    let filename = "day1.txt"
    content <- readFile(filename)
    let replacements = [("one", "o1e"), ("two", "t2o"), ("three", "t3e"), ("four", "f4r"), ("five", "f5e"), ("six", "s6x"), ("seven", "s7n"), ("eight", "e8t"), ("nine", "n9e")]
    let contentLines = map nums (lines content)
    --let withNamedNumbers = map replacePairs replacements contentLines
    let withNamedNumbers = replacePairsInAllStrings replacements contentLines
    let correctNums = map firstAndLast withNamedNumbers
    let total = sum (map read correctNums)
    putStrLn $ show total


