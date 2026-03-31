
-- Exercise 1.1 --
addThree :: (Int, Int, Int) -> Int
addThree (x, y, z) = x + y + z

addThreeCurried :: Int -> Int -> Int -> Int
addThreeCurried x y z = x + y + z

-- Exercise 2.1 --
sumMapped :: (Int -> Int) -> Int -> Int -> Int
sumMapped f x y = sum (map f [x .. y])


-- Exercise 2.2 --
composition :: (b -> c) -> (a -> b) -> a -> c
composition f g x = f (g x)


-- Exercise 2.3 --
composeList :: [a -> a] -> a -> a
composeList [] x = x
composeList (f : fs) x = f (composeList fs x)


-- Exercise 2.4 --
sumList :: (Num a) => [a] -> a
sumList [] = 0
sumList (x : xs) = x + sumList xs


-- Exercise 2.5 --
map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x : xs) = f x : map2 f xs


-- Exercise 2.6 --
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x : xs)
  | p x = x : filterList p xs
  | otherwise = filterList p xs


-- Exercise 2.7 --
foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _ z [] = z
foldrList f z (x : xs) = f x (foldrList f z xs)

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList _ z [] = z
foldlList f z (x : xs) = foldlList f (f z x) xs


-- Exercise 2.8 --
data Arb
  = Leaf
  | Node Integer Arb Arb
  deriving (Show, Eq)

tree1 :: Arb
tree1 = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

mapPreOrder :: (Integer -> a) -> Arb -> [a]
mapPreOrder _ Leaf = []
mapPreOrder f (Node val left right) = [f val] ++ mapPreOrder f left ++ mapPreOrder f right

mapInOrder :: (Integer -> a) -> Arb -> [a]
mapInOrder _ Leaf = []
mapInOrder f (Node val left right) = mapInOrder f left ++ [f val] ++ mapInOrder f right

mapPostOrder :: (Integer -> a) -> Arb -> [a]
mapPostOrder _ Leaf = []
mapPostOrder f (Node val left right) = mapPostOrder f left ++ mapPostOrder f right ++ [f val]


-- Exercise 2.9 --
traverseTree :: (Arb -> [Integer]) -> (Integer -> a) -> Arb -> [a]
traverseTree strategy f tree = map2 f (strategy tree)

preOrderStrategy :: Arb -> [Integer]
preOrderStrategy Leaf = []
preOrderStrategy (Node val left right) = [val] ++ preOrderStrategy left ++ preOrderStrategy right

-- Exercise 3.1 --
sortByCmp :: (a -> a -> Bool) -> [a] -> [a]
sortByCmp _ [] = []
sortByCmp cmp (x : xs) = insertByCmp cmp x (sortByCmp cmp xs)

insertByCmp :: (a -> a -> Bool) -> a -> [a] -> [a]
insertByCmp _ x [] = [x]
insertByCmp cmp x (y : ys)
  | cmp x y = x : y : ys
  | otherwise = y : insertByCmp cmp x ys


-- Exercise 3.2 --
data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)

isLeft :: MyEither a b -> Bool
isLeft (MyLeft _) = True
isLeft _ = False

myEither :: (a -> c) -> (b -> c) -> MyEither a b -> c
myEither f _ (MyLeft x) = f x
myEither _ g (MyRight y) = g y

-- Exercise 3.3 --
data BST a = Empty | BNode a (BST a) (BST a) deriving (Show, Eq)

insertGen :: (Ord a) => a -> BST a -> BST a
insertGen x Empty = BNode x Empty Empty
insertGen x (BNode val left right)
  | x < val = BNode val (insertGen x left) right
  | x > val = BNode val left (insertGen x right)
  | otherwise = BNode val left right


-- Exercise 3.4 --
searchSeq :: (Eq a) => a -> [a] -> Bool
searchSeq _ [] = False
searchSeq target (x : xs)
  | target == x = True
  | otherwise = searchSeq target xs

searchSeqFold :: (Eq a) => a -> [a] -> Bool
searchSeqFold target = foldrList (\x acc -> x == target || acc) False

searchBin :: (Ord a) => a -> BST a -> Bool
searchBin _ Empty = False
searchBin target (BNode val left right)
  | target == val = True
  | target < val = searchBin target left
  | otherwise = searchBin target right

searchBinFold :: (Ord a) => a -> [a] -> Bool
searchBinFold target = foldrList (\x acc -> if target == x then True else if target < x then False else acc) False
