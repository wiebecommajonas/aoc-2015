import System.Environment
import Data.List

data Direction = North | East | South | West

dir :: Char -> Direction
dir c = case c of
          '^' -> North
          '>' -> East
          'v' -> South
          '<' -> West

walkDir (x,y) dir = case dir of
  North -> (x,y-1)
  South -> (x,y+1)
  West  -> (x-1,y)
  East  -> (x+1,y)

countHouses :: [Direction] -> Int
countHouses dirs = walk (0,0) 1 [(0,0)] dirs
  where
    walk _ count _ [] = count
    walk pos count locs (d:ds) = newWalk (walkDir pos d) count locs ds
    newWalk pos count locs ds
      | pos `elem` locs = walk pos count locs ds
      | otherwise       = walk pos (count + 1) (pos:locs) ds

countHousesDual :: [Direction] -> [Direction] -> Int
countHousesDual ads bds = walk (0,0) (0,0) 1 [(0,0)] ads bds
  where
    walk _ _ count _ [] [] = count
    walk apos bpos count locs (ad:ads) (bd:bds) = newWalk (walkDir apos ad) (walkDir bpos bd) count locs ads bds
    newWalk apos bpos count locs ads bds
      | apos `elem` locs && bpos `elem` locs = walk apos bpos  count               locs  ads bds
      | apos `elem` locs                     = walk apos bpos (count+1) (     bpos:locs) ads bds
      | bpos `elem` locs                     = walk apos bpos (count+1) (apos     :locs) ads bds
      | apos == bpos                         = walk apos bpos (count+1) (     bpos:locs) ads bds
      | otherwise                            = walk apos bpos (count+2) (apos:bpos:locs) ads bds

filterIndex :: (Int -> Bool) -> [a] -> [a]
filterIndex p xs = map snd . filter (p . fst) . zip [0..] $ xs

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 3 Part 1: " ++ show (countHouses . map dir . head . lines $ contents)
  putStrLn $ "Solution Day 3 Part 2: " ++ show ((\x -> countHousesDual (filterIndex even x) (filterIndex odd x)). map dir . head . lines $ contents)
