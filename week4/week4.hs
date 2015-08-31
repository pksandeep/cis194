import Data.List ((\\))
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' =  foldr  (\x y -> (x-2) * y ) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n  = sum . filter even . takeWhile (/= 1) . iterate  ( \x -> if even x then x `div` 2 else 3*x + 1) $ n

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf 

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a ( Node n Leaf x Leaf ) = Node (n+1) (insert a Leaf) x Leaf
insert a ( Node n Leaf x r ) = Node n (insert a Leaf) x r
insert a ( Node n l x Leaf ) = Node n l x (insert a Leaf)
insert a ( Node n l@(Node lc ll lx lr)  x r@(Node rc rl rx rr))
   | lc < rc = Node n (insert a l) x r
   | rc < lc = Node n l x (insert a r)
   | otherwise = Node (h+1) ti x r
 where ti@(Node h il ix ir ) = insert a l

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y ) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> ) []
--

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) $ [1..n] \\ (  map (\(x,y) -> x+y+2*x*y ) . filter (\(x,y) -> x <= y && x+y+2*x*y <= n ) $ cartProd  [1..n] [1..n] )

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

