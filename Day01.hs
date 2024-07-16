import System.Environment
import Data.List

level :: String -> Int
level input = (+) (negate . length . filter (== ')') $ input) (length . filter (== '(') $ input)

firstBasement :: String -> Int
firstBasement input = fstbsm 0 0 input
  where
    fstbsm (-1) idx _ = idx
    fstbsm level idx ('(':xs) = fstbsm (level + 1) (idx + 1) xs
    fstbsm level idx (')':xs) = fstbsm (level - 1) (idx + 1) xs

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 1 Part 1: " ++ show (level contents)
  putStrLn $ "Solution Day 1 Part 2: " ++ show (firstBasement contents)

