import System.Environment
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Text.Read hiding (choice)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec hiding (choice)

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)
newtype Range = Range (Coord, Coord) deriving Show
data Instruction = Toggle Range | TurnOff Range | TurnOn Range deriving Show
type Lights = Map.Map Coord Bool

instance Read Coord where
  readPrec = do
    x <- readPrec :: ReadPrec Int
    lift $ char ','
    y <- readPrec :: ReadPrec Int
    return $ Coord (x, y)

instance Read Instruction where
  readPrec = do
    begin <- lift $ choice [string "toggle", string "turn off", string "turn on"]
    lift $ skipSpaces
    from <- readPrec :: ReadPrec Coord
    lift $ skipSpaces
    lift $ string "through"
    lift $ skipSpaces
    to <- readPrec :: ReadPrec Coord
    case begin of
      "toggle" -> return $ Toggle (Range (from, to))
      "turn off" -> return $ TurnOff (Range (from, to))
      "turn on" -> return $ TurnOn (Range (from, to))

range2List :: Range -> [Coord]
range2List (Range (Coord (rx1,ry1), Coord (rx2,ry2))) = [Coord (x,y) | x <- [rx1..rx2], y <- [ry1..ry2]]

initLights :: Lights
initLights = Map.empty

index2Coord :: Int -> Coord
index2Coord idx = Coord (idx `rem` 1000, idx `div` 1000)

toggle :: [Coord] -> Lights -> Lights
toggle []     ls = ls
toggle (r:rs) ls = toggle rs (Map.insertWith (\ a b -> not ( const b a )) r True ls)

turnon :: [Coord] -> Lights -> Lights
turnon []     ls = ls
turnon (r:rs) ls = turnon rs (Map.insert r True ls)

turnoff :: [Coord] -> Lights -> Lights
turnoff []     ls = ls
turnoff (r:rs) ls = turnoff rs (Map.insert r False ls)

followInstructions :: Lights -> [Instruction] -> Lights
followInstructions ls [] = ls
followInstructions ls (i:is) = followInstructions newLs is
  where newLs = case i of
                 Toggle r -> toggle (range2List r) ls
                 TurnOn r -> turnon (range2List r) ls
                 TurnOff r -> turnoff (range2List r) ls

type Lights2 = Map.Map Coord Int

initLights2 :: Lights2
initLights2 = Map.empty

toggle2 :: [Coord] -> Lights2 -> Lights2
toggle2 []     ls = ls
toggle2 (r:rs) ls = toggle2 rs (Map.insertWith (+) r 2 ls)

turnon2 :: [Coord] -> Lights2 -> Lights2
turnon2 []     ls = ls
turnon2 (r:rs) ls = turnon2 rs (Map.insertWith (+) r 1 ls)

turnoff2 :: [Coord] -> Lights2 -> Lights2
turnoff2 []     ls = ls
turnoff2 (r:rs) ls = turnoff2 rs (Map.insertWith (\_ v -> if v == 0 then 0 else v - 1) r 0 ls)

followInstructions2 :: Lights2 -> [Instruction] -> Lights2
followInstructions2 ls [] = ls
followInstructions2 ls (i:is) = followInstructions2 newLs is
  where newLs = case i of
                 Toggle r -> toggle2 (range2List r) ls
                 TurnOn r -> turnon2 (range2List r) ls
                 TurnOff r -> turnoff2 (range2List r) ls

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  -- putStrLn $ "Solution Day 6 Part 1: " ++ show (foldl (\ acc v -> if v then acc + 1 else acc) 0 . followInstructions initLights . map (read :: String -> Instruction) . lines $ contents)
  putStrLn $ "Solution Day 6 Part 2: " ++ show (foldl (+) 0  . followInstructions2 initLights2 . map (read :: String -> Instruction) . lines $ contents)


