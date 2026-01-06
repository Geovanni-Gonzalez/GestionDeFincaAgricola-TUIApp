module Main where

import Menu
import Files
import Types
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Cargando sistema..."
    state <- loadState
    runApp state
