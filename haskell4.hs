import Control.Monad.State
import Data.Void (Void)
import System.Random
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

-- Exercise 0.1
data Expr
  = Var String
  | Num Int
  | BinOp Char Expr Expr
  | Lam String Expr
  | App Expr Expr
  deriving (Show, Eq)

type Parser = MP.Parsec Void String

sc :: Parser ()
sc = L.space MPC.space1 (L.skipLineComment "--") MP.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pIdentifier :: Parser String
pIdentifier = lexeme $ do
  first <- MPC.letterChar MP.<|> MPC.char '_'
  rest <- MP.many (MPC.alphaNumChar MP.<|> MPC.char '_')
  return (first : rest)

pNumber :: Parser Expr
pNumber = Num <$> lexeme L.decimal

pVar :: Parser Expr
pVar = Var <$> pIdentifier

pParens :: Parser Expr
pParens = MP.between (symbol "(") (symbol ")") pExpr

pExpr :: Parser Expr
pExpr = MP.try pLam MP.<|> pApp

pLam :: Parser Expr
pLam = do
  param <- MP.try pIdentifier
  _ <- symbol "->"
  body <- pExpr
  return (Lam param body)

pApp :: Parser Expr
pApp = do
  f <- pArith
  args <- MP.many (MP.try pAtom)
  return (foldl App f args)

pArith :: Parser Expr
pArith = do
  left <- pAtom
  rest left
  where
    rest acc =
      do
        op <- lexeme (MP.oneOf "+-*")
        rhs <- pAtom
        rest (BinOp op acc rhs)
        MP.<|> return acc

pAtom :: Parser Expr
pAtom = pParens MP.<|> pNumber MP.<|> pVar

parseExpr :: String -> Either String Expr
parseExpr input =
  case MP.parse (sc *> pExpr <* MP.eof) "<input>" input of
    Left err -> Left (MP.errorBundlePretty err)
    Right ast -> Right ast

-- ghci> parseExpr "x"
-- Right (Var "x")

-- ghci> parseExpr "x + 3"
-- Right (BinOp '+' (Var "x") (Num 3))

-- ghci> parseExpr "x -> x + 3"
-- Right (Lam "x" (BinOp '+' (Var "x") (Num 3)))

-- ghci> parseExpr "(x -> x + 3) 7"
-- Right (App (Lam "x" (BinOp '+' (Var "x") (Num 3))) (Num 7))

-- Exercise 0.2
type Stack = [Int]

push :: Int -> State Stack ()
push x = modify (x :)

pop :: State Stack (Maybe Int)
pop = do
  s <- get
  case s of
    [] -> return Nothing
    (x : xs) -> put xs >> return (Just x)

stackProduct :: State Stack Int
stackProduct = gets product

randomStackProduct :: IO Int
randomStackProduct = do
  n1 <- randomRIO (1, 100)
  n2 <- randomRIO (1, 100)

  let computation = do
        push n1
        push n2
        stackProduct

  let (result, _) = runState computation []
  return result

-- ghci> randomStackProduct
-- 644