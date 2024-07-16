import System.Environment
import Data.List

threeVowels :: String -> Bool
threeVowels str = tv 0 str
  where
    tv 3 _ = True
    tv _ [] = False
    tv counter (x:xs) | x `elem` "aeiou" = tv (counter+1) xs
                      | otherwise        = tv counter xs

doubleLetter :: String -> Bool
doubleLetter (x:xs) = dl x xs
  where
    dl _ [] = False
    dl c (x:xs) | c == x = True
                | otherwise = dl x xs

checkBlacklist :: String -> Bool
checkBlacklist [] = True
checkBlacklist str@(x:xs) = (all (\x -> not $ x `isPrefixOf` str) $ blacklist) && checkBlacklist xs
  where
    blacklist = ["ab", "cd", "pq", "xy"]

nice :: String -> Bool
nice str = threeVowels str && doubleLetter str && checkBlacklist str

pairTwice :: String -> Bool
pairTwice str@(x:y:xs) = (any (isPrefixOf [x,y]) . tails $ xs) || pairTwice (y:xs)
pairTwice _ = False

surrounded :: String -> Bool
surrounded (x:rest@(_:z:xs)) | x == z = True
                             | otherwise = surrounded rest
surrounded _ = False

newNice :: String -> Bool
newNice str = pairTwice str && surrounded str

main :: IO()
main = do
  args <- getArgs
  contents <- readFile $ head args
  
  putStrLn $ "Solution Day 5 Part 1: " ++ show (length . filter nice . words $ contents)
  putStrLn $ "Solution Day 5 Part 2: " ++ show (length . filter newNice . words $ contents)


