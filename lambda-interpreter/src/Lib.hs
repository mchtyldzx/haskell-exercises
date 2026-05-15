module Lib
    ( Term(..)
    , Id
    , reduce
    , reduceFor
    , parseTerm
    ) where

import Data.List (elem)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Id = String

-- Exercise 0.1
data Term = Var Id
          | App Term Term
          | Lambda Id Term 
          deriving (Eq)

instance Show Term where
    show (Var x) = x
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (Lambda x t) = "(\\" ++ x ++ "." ++ show t ++ ")"

-- ghci> Lambda "x" (Lambda "y" (Var "x"))
-- (\x.(\y.x))

-- Exercise 0.2
subst :: Id -> Term -> Term -> Term
subst id term (Var id') | id == id' = term
                        | otherwise = Var id'
subst id term (App term1 term2) = App (subst id term term1) (subst id term term2)
subst id term (Lambda id' term') | id == id' = Lambda id' term'
                                 | otherwise = Lambda id' (subst id term term')

-- ghci> subst "x" (Var "y") (Var "x")
-- y
-- ghci> subst "y" (Var "z") (App (Var "x") (Var "y"))
-- (x z)
-- ghci> subst "x" (Lambda "z" (Var "z")) (Lambda "x" (App (Var "y") (Var "x")))
-- (\x.(y x))

-- Exercise 0.3
remove :: Id -> [Id] -> [Id]
remove _ [] = []
remove id (hd:tl) | id == hd = remove id tl
                  | otherwise = hd : remove id tl

-- ghci> remove "x" ["y", "x", "z", "x"]
-- ["y","z"]

-- Exercise 0.4
free :: Term -> [Id]
free (Var id) = [id]
free (App term1 term2) = free term1 ++ free term2
free (Lambda id term) = remove id (free term)

-- ghci> free (Lambda "x" (App (Var "y") (Var "x")))
-- ["y"]

-- Exercise 0.5
vars :: Term -> [Id]
vars (Var id) = [id]
vars (App term1 term2) = vars term1 ++ vars term2
vars (Lambda id term) = id : vars term

-- ghci> vars (Lambda "x" (App (Var "y") (Var "x")))
-- ["x","y","x"]

-- Exercise 0.6
fresh' :: [Id] -> Int -> Id
fresh' ids index = if ("n" ++ show index) `elem` ids 
                   then fresh' ids (index + 1)
                   else "n" ++ show index

fresh :: [Id] -> Id
fresh ids = fresh' ids 0

-- ghci> fresh ["n0", "n1", "n2"]
-- "n3"

-- Exercise 0.7
casubst :: Id -> Term -> Term -> [Id] -> Term
casubst id term (Var id') _ | id == id' = term
                            | otherwise = Var id'
casubst id term (App term1 term2) avoid = App (casubst id term term1 avoid) (casubst id term term2 avoid)
casubst id term (Lambda id' term') avoid 
    | id == id' = Lambda id' term'
    | id' `elem` (free term) = 
        let id'' = fresh avoid 
            newTerm' = casubst id' (Var id'') term' (id'' : avoid)
        in Lambda id'' (casubst id term newTerm' (id'' : avoid))
    | otherwise = Lambda id' (casubst id term term' avoid)

-- ghci> casubst "y" (Var "x") (Lambda "x" (Var "y")) []
-- (\n0.x)

-- Exercise 0.8
reduce1' :: Term -> [Id] -> Maybe Term
reduce1' (Var _) _ = Nothing
reduce1' (App (Lambda id term) term') avoid = Just (casubst id term' term avoid)
reduce1' (App term1 term2) avoid = case reduce1' term1 avoid of
    Just term1' -> Just (App term1' term2)
    Nothing -> case reduce1' term2 avoid of
        Just term2' -> Just (App term1 term2')
        Nothing -> Nothing
reduce1' (Lambda id term) avoid = case reduce1' term avoid of
    Just term' -> Just (Lambda id term')
    Nothing -> Nothing

reduce1 :: Term -> Maybe Term
reduce1 t = reduce1' t (vars t)

-- ghci> reduce1 (App (Lambda "x" (Var "x")) (Var "y"))
-- Just y

-- Exercise 0.9
reduce :: Term -> Term
reduce term = case reduce1 term of
    Nothing -> term
    Just term' -> reduce term'

-- ghci> reduce (App (Lambda "x" (Var "x")) (App (Lambda "z" (Var "z")) (Var "y")))
-- y

-- Exercise 0.10
reduceFor :: Int -> Term -> Term
reduceFor 0 term = term
reduceFor n term = case reduce1 term of
    Nothing -> term
    Just term' -> reduceFor (n - 1) term'

-- ghci> let omega = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
-- ghci> reduceFor 3 omega
-- ((\x.(x x)) (\x.(x x)))

-- Exercise 0.11
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pVar :: Parser Term
pVar = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

pLambda :: Parser Term
pLambda = do
    _ <- symbol "\\"
    varId <- lexeme ((:) <$> letterChar <*> many alphaNumChar)
    _ <- symbol "."
    body <- pTerm
    return $ Lambda varId body

pAtom :: Parser Term
pAtom = between (symbol "(") (symbol ")") pTerm
    <|> pLambda
    <|> pVar

pTerm :: Parser Term
pTerm = do
    terms <- some pAtom
    return $ foldl1 App terms

parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm = parse (sc *> pTerm <* eof) ""

-- ghci> parseTerm "(\\x.x) y"
-- Right ((\x.x) y)

-- Stack Exec Test Commands & Outputs
-- 
-- 1. Basic Lambda Application
-- > stack exec lambda-interpreter-exe "(\x.x) y"
-- Parsed Expression : ((\x.x) y)
-- Result            : y
-- 
-- 2. Multiple Application (Currying test)
-- > stack exec lambda-interpreter-exe "((\x.\y.x) a) b"
-- Parsed Expression : (((\x.(\y.x)) a) b)
-- Result            : a
-- 
-- 3. Nested Lambda and Free Variable test (Capture-avoidance)
-- > stack exec lambda-interpreter-exe "(\x.\y.x y) z"
-- Parsed Expression : ((\x.(\y.(x y))) z)
-- Result            : (\y.(z y))
-- 
-- 4. Infinite Loop (Omega) with specific number of reduction steps
-- > stack exec lambda-interpreter-exe "(\x.(x x)) (\x.(x x))" 3
-- Parsed Expression : ((\x.(x x)) (\x.(x x)))
-- Result            : ((\x.(x x)) (\x.(x x)))