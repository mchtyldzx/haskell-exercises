
-- ============================================
-- 1. AVL TREES
-- ============================================

data Arb
  = Leaf
  | Node Integer Arb Arb
  deriving (Show, Eq)

tree1 :: Arb
tree1 = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 (Node 6 Leaf Leaf) (Node 9 Leaf Leaf))

treeNotAVL :: Arb
treeNotAVL = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf

-- height :: Arb -> Int
height :: Arb -> Int
height Leaf = 0
height (Node _ l r) = 1 + max (height l) (height r)

--ghci> height tree1
--3

-- isAVL :: Arb -> Bool
isAVL :: Arb -> Bool
isAVL Leaf = True
isAVL (Node _ l r) = abs (height l - height r) <= 1 && isAVL l && isAVL r

--ghci> isAVL tree1
--True
--ghci> isAVL treeNotAVL
--False

-- rotateLeft :: Arb -> Arb
rotateLeft :: Arb -> Arb
rotateLeft (Node x t1 (Node y t2 t3)) = Node y (Node x t1 t2) t3
rotateLeft t = t

--ghci> rotateLeft (Node 1 Leaf (Node 2 Leaf Leaf))
--Node 2 (Node 1 Leaf Leaf) Leaf

-- rotateRight :: Arb -> Arb
rotateRight :: Arb -> Arb
rotateRight (Node y (Node x t1 t2) t3) = Node x t1 (Node y t2 t3)
rotateRight t = t

--ghci> rotateRight (Node 2 (Node 1 Leaf Leaf) Leaf)
--Node 1 Leaf (Node 2 Leaf Leaf)

-- doubleRotateLeft :: Arb -> Arb
doubleRotateLeft :: Arb -> Arb
doubleRotateLeft (Node x t1 (Node z (Node y t2 t3) t4)) = rotateLeft (Node x t1 (rotateRight (Node z (Node y t2 t3) t4)))
doubleRotateLeft t = t

-- doubleRotateRight :: Arb -> Arb
doubleRotateRight :: Arb -> Arb
doubleRotateRight (Node z (Node x t1 (Node y t2 t3)) t4) = rotateRight (Node z (rotateLeft (Node x t1 (Node y t2 t3))) t4)
doubleRotateRight t = t


-- balance :: Arb -> Arb
balance :: Arb -> Arb
balance Leaf = Leaf
balance (Node val l r)
    | bf > 1 && balanceFactor l >= 0 = rotateRight (Node val l r)
    | bf > 1 && balanceFactor l < 0  = doubleRotateRight (Node val l r)
    | bf < -1 && balanceFactor r <= 0 = rotateLeft (Node val l r)
    | bf < -1 && balanceFactor r > 0  = doubleRotateLeft (Node val l r)
    | otherwise = Node val l r
  where
    bf = balanceFactor (Node val l r)
    balanceFactor Leaf = 0
    balanceFactor (Node _ left right) = height left - height right


-- insertAVL :: Arb -> Integer -> Arb
insertAVL :: Arb -> Integer -> Arb
insertAVL Leaf x = Node x Leaf Leaf
insertAVL (Node val l r) x
    | x == val  = Node val l r
    | x < val   = balance (Node val (insertAVL l x) r)
    | otherwise = balance (Node val l (insertAVL r x))

--ghci> isAVL (insertAVL treeNotAVL 6)
--True

-- removeAVL :: Arb -> Integer -> Arb 
-- (Need minBST and removeMax from lab3)
maxBST :: Arb -> Integer
maxBST Leaf = error "Tree is empty, couldnt find max"
maxBST (Node val _ Leaf) = val
maxBST (Node _ _ right) = maxBST right

removeMax :: Arb -> Arb
removeMax Leaf = error "Tree is empty!"
removeMax (Node _ left Leaf) = left
removeMax (Node val left right) = balance (Node val left (removeMax right))

removeAVL :: Arb -> Integer -> Arb
removeAVL Leaf _ = Leaf
removeAVL (Node val l r) x
    | x < val   = balance (Node val (removeAVL l x) r)
    | x > val   = balance (Node val l (removeAVL r x))
    | otherwise = 
        case (l, r) of
            (Leaf, _) -> r
            (_, Leaf) -> l
            _         -> let maxLeft = maxBST l
                         in balance (Node maxLeft (removeMax l) r)


-- ============================================
-- 2. NATURAL NUMBERS (Unary Representation)
-- ============================================

data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Exercise 2.1
-- add :: Nat -> Nat -> Nat
add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

--ghci> add (Succ (Succ Zero)) (Succ Zero)
--Succ (Succ (Succ Zero))

-- mult :: Nat -> Nat -> Nat
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ x) y = add y (mult x y)

--ghci> mult (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
--Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

-- exp :: Nat -> Nat -> Nat
expo :: Nat -> Nat -> Nat
expo _ Zero = Succ Zero
expo x (Succ y) = mult x (expo x y)

--ghci> expo (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))
--Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))

-- comp :: Nat -> Nat -> Bool -- comp x y means "x < y"
comp :: Nat -> Nat -> Bool
comp _ Zero = False
comp Zero (Succ _) = True
comp (Succ x) (Succ y) = comp x y

--ghci> comp (Succ Zero) (Succ (Succ Zero))
--True

-- sub :: Nat -> Nat -> Nat
sub :: Nat -> Nat -> Nat
sub Zero _ = Zero
sub x Zero = x
sub (Succ x) (Succ y) = sub x y

--ghci> sub (Succ (Succ (Succ Zero))) (Succ Zero)
--Succ (Succ Zero)

-- div :: Nat -> Nat -> Nat
division :: Nat -> Nat -> Nat
division _ Zero = error "Division by zero"
division x y
    | comp x y = Zero
    | otherwise = Succ (division (sub x y) y)

--ghci> division (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ Zero))
--Succ (Succ Zero)

-- rem :: Nat -> Nat -> Nat
remainder :: Nat -> Nat -> Nat
remainder _ Zero = error "Division by zero"
remainder x y
    | comp x y = x
    | otherwise = remainder (sub x y) y

--ghci> remainder (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
--Succ Zero

-- convert :: Nat -> Int
convert :: Nat -> Int
convert Zero = 0
convert (Succ n) = 1 + convert n

--ghci> convert (Succ (Succ (Succ Zero)))
--3

-- convert’ :: Int -> Nat
convert' :: Int -> Nat
convert' 0 = Zero
convert' n | n > 0 = Succ (convert' (n - 1))
           | otherwise = error "Negative numbers not supported"

--ghci> convert' 4
--Succ (Succ (Succ (Succ Zero)))


-- ============================================
-- 3. BOOLEAN EXPRESSIONS
-- ============================================

-- Exercise 3.1. 
data BoolExpr
    = Var String
    | T
    | F
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    deriving (Show, Eq)

-- Exercise 3.2. Evaluation
type Env = [(String, Bool)]

eval :: Env -> BoolExpr -> Bool
eval _ T = True
eval _ F = False
eval env (Var x) = case lookup x env of
                     Just val -> val
                     Nothing -> error ("Variable " ++ x ++ " not found")
eval env (Not e) = not (eval env e)
eval env (And e1 e2) = (eval env e1) && (eval env e2)
eval env (Or e1 e2) = (eval env e1) || (eval env e2)

--ghci> eval [("x", True), ("y", False)] (And (Var "x") (Not (Var "y")))
--True

-- Exercise 3.3. Simplification
simplify :: BoolExpr -> BoolExpr
simplify (Not T) = F
simplify (Not F) = T
simplify (Not (Not e)) = simplify e
simplify (And T e) = simplify e
simplify (And e T) = simplify e
simplify (And F _) = F
simplify (And _ F) = F
simplify (Or T _) = T
simplify (Or _ T) = T
simplify (Or F e) = simplify e
simplify (Or e F) = simplify e
simplify (Not e) = Not (simplify e)
simplify (And e1 e2) = And (simplify e1) (simplify e2)
simplify (Or e1 e2) = Or (simplify e1) (simplify e2)
simplify e = e

--ghci> simplify (And T (Or (Var "x") F))
--Var "x"

-- Exercise 3.4. Conjunctive Normal Form (CNF)
-- (Conjunctive Normal Form: AND of ORs)
toNNF :: BoolExpr -> BoolExpr
toNNF (Not (Not e)) = toNNF e
toNNF (Not (And e1 e2)) = Or (toNNF (Not e1)) (toNNF (Not e2)) -- De Morgan
toNNF (Not (Or e1 e2)) = And (toNNF (Not e1)) (toNNF (Not e2)) -- De Morgan
toNNF (And e1 e2) = And (toNNF e1) (toNNF e2)
toNNF (Or e1 e2) = Or (toNNF e1) (toNNF e2)
toNNF (Not e) = Not (toNNF e)
toNNF e = e

distribute :: BoolExpr -> BoolExpr
distribute (Or (And e1 e2) e3) = And (distribute (Or e1 e3)) (distribute (Or e2 e3))
distribute (Or e1 (And e2 e3)) = And (distribute (Or e1 e2)) (distribute (Or e1 e3))
distribute (Or e1 e2) = Or (distribute e1) (distribute e2)
distribute (And e1 e2) = And (distribute e1) (distribute e2)
distribute e = e

toCNF :: BoolExpr -> BoolExpr
toCNF e = distribute (toNNF e)

--ghci> toCNF (Not (And (Var "a") (Var "b")))
--Or (Not (Var "a")) (Not (Var "b"))
