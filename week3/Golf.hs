module Golf where
import Data.List
skips :: [a] -> [[a]]
skips xs =  map (\(a,b) -> takeNth a xs) $ zip [1..] xs

takeNth :: Int -> [a] -> [a]
takeNth n xs = map snd $ filter (\(a,b) -> a `mod` n == 0 ) $ zip [1..] xs

localMaxima :: [Integer] -> [Integer]
localMaxima n@(x:y:z:xs) = map (\(a,b,c) -> b ) $ filter (\(a,b,c) ->  b > a && b > c ) $ zip3 n  (tail n)  (tail . tail $  n)

histogram :: [Integer] -> String
histogram xs = unlines . reverse $ "0123456789" : "=========="  :unfoldr  printHisto  (frequencyDigits xs)

frequencyDigits :: [Integer] -> [Integer]
frequencyDigits xs = map (\n -> toInteger (length n - 1 )) $ group . sort $ (xs ++ [0..9])

printHisto :: [Integer] -> Maybe (String, [Integer])
printHisto xs
  | sum xs == 0 = Nothing
  | otherwise = Just (histo,counts)
                  where histo = map (\n -> if n> 0 then '*' else ' ' ) xs
                        counts = map (\n -> if n > 0 then n-1 else 0) xs 



