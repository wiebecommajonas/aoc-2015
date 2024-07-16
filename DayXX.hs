import System.Environment
import Data.List

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day X Part 1: " ++ show (contents)
  putStrLn $ "Solution Day X Part 2: " ++ show (contents)


