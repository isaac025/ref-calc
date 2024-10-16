module Main where

import Control.Monad (when)
import Eval
import Parser
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

quit :: String -> IO ()
quit c =
    when (c == "quit" || c == "q") $ do
        exitSuccess

repl :: IO ()
repl = do
    putStr "rc> "
    hFlush stdout
    c <- getLine
    quit c
    let res = parseExpr c
    case res of
        Left err -> putStrLn err >> repl
        Right val -> do
            let t = tc val
             in when (isType t) $ do print' (eval val) t >> repl
  where
    print' :: Value -> Type -> IO ()
    print' v t = putStrLn (show v <> " : " <> show t)
    isType :: Type -> Bool
    isType _ = True

evalF :: FilePath -> IO ()
evalF = undefined

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [f] -> evalF f
        _ -> putStrLn "Usage: rc [FILE]"
