import System.Environment
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Crypto.Hash.MD5 as MD5

findKey :: String -> Int
findKey input = fh input 1
  where
    fh input num
      | (BS.take 5 . MD5.hash . BSU.fromString $ input ++ show num) == BS.replicate 5 0 = num
      | otherwise                                                                = fh input (num+1)

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 4 Part 1: " ++ show (findKey . head . lines $ contents)
  putStrLn $ "Solution Day 4 Part 2: " ++ show (contents)
