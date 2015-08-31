toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise =  toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther xs = reverse $ doubleEveryOtherHelper False  (reverse xs)

doubleEveryOtherHelper :: Bool -> [Integer] -> [Integer]
doubleEveryOtherHelper _  [] = []
doubleEveryOtherHelper c  (x:xs) = (if c then (x*2) else x) : doubleEveryOtherHelper (not c) xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = sumDigits ( doubleEveryOther ( toDigits  n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =  ( hanoi (n-1) a c b ) ++ ((a,b) : hanoi (n-1) c b a)


