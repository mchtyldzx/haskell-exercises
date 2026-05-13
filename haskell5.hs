
import Data.Char (toUpper)
import System.IO
import System.Environment
import System.Random (newStdGen, randomRs)

-- Exercise 0.1: 
unitValue :: ()
unitValue = ()

{- 
ghci> :t ()
() :: ()
-}

-- Exercise 0.2:
threeActions :: IO ()
threeActions = putStrLn "First" >> putStrLn "Second" >> putStrLn "Third"

{- 
ghci> :info (>>)
infixl 1 >>  -- Associates to the LEFT
ghci> threeActions
First
Second
Third
-}

-- Exercise 0.3:
greetBind :: IO ()
greetBind = 
    putStrLn "First Name:" >>= \first ->
    getLine >>= \fValue ->
    putStrLn "Last Name:" >>= \last ->
    getLine >>= \lValue ->
    putStrLn ("Hello, " ++ fValue ++ " " ++ lValue ++ "!")

{- 
ghci> greetBind
First Name:
Mucahit
Last Name:
Yildiz
Hello, Mucahit Yildiz!
-}

-- Exercise 0.4:
greetDo :: IO ()
greetDo = do
    putStrLn "First Name:"
    first <- getLine
    putStrLn "Last Name:"
    last <- getLine
    putStrLn ("Hello, " ++ first ++ " " ++ last ++ "!")

{- 
ghci> greetDo
First Name:
Mucahit
Last Name:
Yildiz
Hello, Mucahit Yildiz!
-}

-- Exercise 0.5:
greetRecursiveBind :: IO ()
greetRecursiveBind = 
    putStrLn "First Name:" >>
    getLine >>= \first ->
    putStrLn "Last Name:" >>
    getLine >>= \last ->
    putStrLn ("Hello, " ++ first ++ " " ++ last ++ "!") >>
    greetRecursiveBind

{- 
ghci> greetRecursiveBind
First Name:
Mucahit
Last Name:
Yildiz
Hello, Mucahit Yildiz!
First Name:
(Repeats indefinitely until Ctrl+C)
-}

-- Exercise 0.7:
greetWithExit :: IO ()
greetWithExit = do
    putStrLn "First Name (empty to exit):"
    f <- getLine
    putStrLn "Last Name (empty to exit):"
    l <- getLine
    if f == "" || l == "" 
        then return () 
        else putStrLn ("Hello, " ++ f ++ " " ++ l ++ "!") >> greetWithExit

{- 
ghci> greetWithExit
First Name (empty to exit):
Mucahit
Last Name (empty to exit):

ghci> (Program ends)
-}

-- Exercise 0.8:
shout :: IO ()
shout = do
    line <- getLine
    putStrLn (map toUpper line) >> shout

{- 
ghci> shout
haskell
HASKELL
-}

-- Exercise 0.10:
displayFile :: FilePath -> IO ()
displayFile path = do
    content <- readFile path
    putStrLn content

{- 
ghci> displayFile "example.txt"
This is a example text file
This is another line for example
-}

-- Exercise 0.14:
guessGame :: Int -> Int -> IO ()
guessGame low high 
    | low == high = putStrLn ("The number is " ++ show low ++ ".")
    | otherwise = do
        let mid = (low + high) `div` 2
        putStrLn ("Is the number >= " ++ show (mid + 1) ++ "? (Da/Nu)")
        answer <- getLine
        case map toUpper answer of
            "DA" -> guessGame (mid + 1) high
            "NU" -> guessGame low mid
            _    -> putStrLn "Nu am inteles." >> guessGame low high 

{- 
ghci> guessGame 1 100
Is the number >= 51? (Da/Nu)
Nu
Is the number >= 26? (Da/Nu)
...
The number is 7.
-}

-- Exercise 0.17:
randomN :: IO ()
randomN = do
    putStrLn "Enter n:"
    nStr <- getLine
    let n = read nStr :: Int
    gen <- newStdGen
    print $ take n (randomRs (1, 100) gen :: [Int])

{- 
ghci> randomN
Enter n:
3
[42, 7, 89]
-}

main :: IO ()
main = greetWithExit
