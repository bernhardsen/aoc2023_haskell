import Data.List.Split

data DiceGame = DiceGame {
  gameId :: Int,
  throws :: [String]
}

diceGameFromDefinition :: String -> DiceGame
diceGameFromDefinition def = DiceGame {
  gameId = read $ last $ splitOn " " $ head $ splitOn ": " def :: Int,
  throws = splitOn "; " $ last $ splitOn ": " def
}

gamePossible :: DiceGame -> Bool
gamePossible game = all id $ map throwPossible $ throws game

throwPossible :: String -> Bool
throwPossible throw = all id $ map diePossible $ splitOn ", " throw

diePossible :: String -> Bool
diePossible dieThrow
    | (color == "red" && count <= 12) || (color == "green" && count <= 13) || (color == "blue" && count <= 14) = True
    | otherwise = False
    where
      color = last (splitOn " " dieThrow)
      count = read (head (splitOn " " dieThrow)) :: Int

powerOfGame :: DiceGame -> Int
powerOfGame game = (maxCount "red" $ throws game) * (maxCount "green" $ throws game) * (maxCount "blue" $ throws game)

getCount :: String -> Int
getCount die = read $ head $ splitOn " " die :: Int

highestValue :: Int -> [Int] -> Int
highestValue current [] = current
highestValue current (x:xs) = if x > current then highestValue x xs else highestValue current xs

maxCount :: String -> [String] -> Int
maxCount color throws = highestValue 0 $ map getCount $ filter colorFilter $ concat $ map (splitOn ", ") throws
    where
      colorFilter = isColor color

isColor :: String -> String -> Bool
isColor color candidate = color == (last $ splitOn " " candidate)

main :: IO()
main = do
    putStrLn "--- Day 2: Cube Conundrum ---"
    part1
    part2

part1 = do
    putStrLn "Part 1:"
    let fileName = "day2.txt"
    content <- readFile(fileName)
    let games = map diceGameFromDefinition $ lines content
    let totalId = sum $ map gameId $ filter gamePossible games
    putStrLn $ "The sum of the ID of the possible games is: " ++ show totalId

part2 = do
    putStrLn "Part 2:"
    content <- readFile("day2.txt")
    let games = map diceGameFromDefinition $ lines content
    let powers = map powerOfGame games
    let totalPower = sum powers
    putStrLn $ "The sum of the power of all the games games is: " ++ show totalPower
