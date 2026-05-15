module Main (main) where

import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)
import Lib (parseTerm, reduce, reduceFor)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "ERROR: Provide a lambda expression."
            
        [exprStr] -> processExpr exprStr Nothing
        
        (exprStr:stepsStr:_) -> do
            let steps = read stepsStr :: Int
            processExpr exprStr (Just steps)

processExpr :: String -> Maybe Int -> IO ()
processExpr exprStr maybeSteps = do
    case parseTerm exprStr of
        Left err -> putStrLn $ "Parse Error:\n" ++ errorBundlePretty err
        Right term -> do
            putStrLn $ "Parsed Expression : " ++ show term
            let result = case maybeSteps of
                            Nothing -> reduce term
                            Just n  -> reduceFor n term
            putStrLn $ "Result            : " ++ show result