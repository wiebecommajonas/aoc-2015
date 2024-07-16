import System.Environment
import Data.List
import Text.Read
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

newtype Present = Present (Int, Int, Int) deriving (Show, Eq, Ord)

area :: Present -> Int
area (Present (l,w,h)) = 2*l*w + 2*w*h + 2*h*l

extra :: Present -> Int
extra (Present (l,w,h)) = minimum [l*w, w*h, h*l]

paper :: Present -> Int
paper p = area p + extra p

volume :: Present -> Int
volume (Present (l,w,h)) = l*w*h

shortestPerimeter :: Present -> Int
shortestPerimeter (Present (l,w,h)) = minimum $ map (*2) [l+w,w+h,h+l]

ribbon :: Present -> Int
ribbon p = shortestPerimeter p + volume p

instance Read Present where
  readPrec = do
    arr <- lift $ sepBy (munch (/= 'x')) (char 'x')
    return $ Present (readInt $ arr !! 0,readInt $ arr !! 1,readInt $ arr !! 2)
    where
      readInt = read :: String -> Int

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 2 Part 1: " ++ show (sum . map paper . map (read :: String -> Present) . words $ contents)
  putStrLn $ "Solution Day 2 Part 2: " ++ show (sum . map ribbon . map (read :: String -> Present) . words $ contents)


