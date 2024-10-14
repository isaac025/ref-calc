module Main where

import Control.Monad (void, when)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

quit :: String -> IO ()
quit c = when c == "quit" || c == "q" pure ()

repl :: IO String
repl = do
    putStr "rc> "
    hFlush stdout
    c <- getLine
    quit c
    repl

eval :: IO ()
eval = undefined

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl
        [f] -> eval f
        _ -> putStrLn "Usage: rc [FILE]"
