import Text.XHtml (height)
-- Ex 1.1 1.2
data MobileDevice
  = Smartphone
  | Laptop
  | Tablet Int String
  deriving (Show)
--ghci> :t Smartphone
--Smartphone :: MobileDevice
--ghci> :t Laptop    
--Laptop :: MobileDevice
--ghci> :t Tablet
--Tablet :: Int -> String -> MobileDevice

-- Ex 1.3
description :: MobileDevice -> String
description (Laptop) = "This is a grey laptop."
description (Tablet _  _) = "This is a red tablet."
description (Smartphone) = "This is a black smartphone."
--ghci> description Laptop 
--"This is a grey laptop."
--ghci> description Tablet 
--"This is a red tablet."
--ghci> description Smartphone
--"This is a black smartphone."

-- Ex 1.4
data Colors
  = Black |Yellow |Red |Grey
  deriving (Show)

data MobileDevice2
  = Smartphone2 Colors
  | Laptop2 Colors
  | Tablet2 Int String Colors
  deriving (Show)

deviceColor :: MobileDevice2 -> Colors
deviceColor (Smartphone2 c) = c
deviceColor (Laptop2 c) = c
deviceColor (Tablet2 _ _ c) = c
--ghci> deviceColor (Smartphone2 Black)     
--Black
--ghci> deviceColor (Tablet2 11 "Apple" Red)
--Red
--ghci> deviceColor (Laptop2 Grey)
--Grey

-- Ex 2.1
data Arb 
  = Leaf 
  | Node Integer Arb Arb 
  deriving (Show, Eq)

tree1 :: Arb
tree1 = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

-- Ex 2.2
minBST :: Arb -> Integer
minBST (Node val Leaf _) = val
minBST (Node _ left _) = minBST left
--ghci> minBST tree1
--1

-- Ex 2.3
maxBST :: Arb -> Integer
maxBST (Node val _ Leaf) = val
maxBST (Node _ _ right) = maxBST right
--ghci> maxBST tree1
--9

-- Ex 2.4
isBST :: Arb -> Bool
isBST Leaf = True
isBST (Node val left right) = 
    isBST left && isBST right && 
    (checkLeft left val) && 
    (checkRight right val)
  where
    checkLeft Leaf _ = True
    checkLeft l v = maxBST l < v
    checkRight Leaf _ = True
    checkRight r v = minBST r > v
--ghci> isBST tree1
--True

-- Ex 2.5
search :: Arb -> Integer -> Bool
search Leaf _ = False
search (Node val left right) x
    | x == val  = True
    | x < val   = search left x
    | otherwise = search right x
--ghci> search tree1 6
--True
--ghci> search tree1 10
--False

-- Ex 2.6
insert :: Arb -> Integer -> Arb
insert Leaf x = Node x Leaf Leaf
insert (Node val left right) x
    | x == val  = Node val left right
    | x < val   = Node val (insert left x) right
    | otherwise = Node val left (insert right x)
--ghci> insert tree1 7
--Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf (Node 7 Leaf Leaf)) (Node 9 Leaf Leaf))

-- Ex 2.7
removeMax :: Arb -> Arb
removeMax (Node _ left Leaf) = left
removeMax (Node val left right) = Node val left (removeMax right)
--ghci> removeMax tree1
--Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf Leaf) Leaf)

-- Ex 2.8
remove :: Arb -> Integer -> Arb
remove Leaf _ = Leaf
remove (Node val left right) x
    | x < val   = Node val (remove left x) right
    | x > val   = Node val left (remove right x)
    | otherwise = 
        case (left, right) of
            (Leaf, _) -> right
            (_, Leaf) -> left
            _         -> let maxLeft = maxBST left
                         in Node maxLeft (removeMax left) right
--ghci> remove tree1 5
--Node 4 (Node 3 (Node 1 Leaf Leaf) Leaf) (Node 8 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

-- Ex 2.9
preOrder :: Arb -> [Integer]
preOrder Leaf = []
preOrder (Node val left right) = val : preOrder left ++ preOrder right

inOrder :: Arb -> [Integer]
inOrder Leaf = []
inOrder (Node val left right) = inOrder left ++ [val] ++ inOrder right

postOrder :: Arb -> [Integer]
postOrder Leaf = []
postOrder (Node val left right) = postOrder left ++ postOrder right ++ [val]
--ghci> preOrder tree1
--[5,3,1,4,8,6,9]
--ghci> inOrder tree1
--[1,3,4,5,6,8,9]
--ghci> postOrder tree1
--[1,4,3,6,9,8,5]
