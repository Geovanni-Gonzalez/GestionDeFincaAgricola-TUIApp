module UIUtils where

import System.IO
import Control.Monad (when)

-- ANSI Escape Codes
res, b, i, u :: String
res = "\ESC[0m"   -- Reset
b   = "\ESC[1m"   -- Bold
i   = "\ESC[3m"   -- Italic
u   = "\ESC[4m"   -- Underline

-- Foreground Colors
red, green, yellow, blue, magenta, cyan, white :: String
red     = "\ESC[31m"
green   = "\ESC[32m"
yellow  = "\ESC[33m"
blue    = "\ESC[34m"
magenta = "\ESC[35m"
cyan    = "\ESC[36m"
white   = "\ESC[37m"

-- Box Drawing Characters
tl, tr, bl, br, hh, vv :: String
tl = "╔"
tr = "╗"
bl = "╚"
br = "╝"
hh = "═"
vv = "║"

-- UI Functions

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

printHeader :: String -> String -> IO ()
printHeader color title = do
    let len = length title + 4
    let line = concat (replicate len hh)
    putStrLn $ color ++ b ++ tl ++ line ++ tr ++ res
    putStrLn $ color ++ b ++ vv ++ "  " ++ title ++ "  " ++ vv ++ res
    putStrLn $ color ++ b ++ bl ++ line ++ br ++ res

printBox :: String -> [String] -> IO ()
printBox color linesArr = do
    let maxLen = maximum (map length linesArr) + 4
    let line = concat (replicate maxLen hh)
    putStrLn $ color ++ tl ++ line ++ tr ++ res
    mapM_ (\l -> putStrLn $ color ++ vv ++ " " ++ l ++ replicate (maxLen - length l - 1) ' ' ++ vv ++ res) linesArr
    putStrLn $ color ++ bl ++ line ++ br ++ res

printError :: String -> IO ()
printError msg = putStrLn $ red ++ b ++ " [ERROR] " ++ res ++ msg

printSuccess :: String -> IO ()
printSuccess msg = putStrLn $ green ++ b ++ " [ÉXITO] " ++ res ++ msg

printInfo :: String -> IO ()
printInfo msg = putStrLn $ cyan ++ b ++ " [INFO] " ++ res ++ msg

-- Custom Banner
printBanner :: IO ()
printBanner = do
    putStrLn $ green ++ b ++ "  _____ _                  " ++ yellow ++ "   _                _ " ++ res
    putStrLn $ green ++ b ++ " |  ___(_)_ __   ___ __ _  " ++ yellow ++ "  / \\   __ _ _ __(_) ___ ___  | | __ _ " ++ res
    putStrLn $ green ++ b ++ " | |_  | | '_ \\ / __/ _` | " ++ yellow ++ " / _ \\ / _` | '__| |/ __/ _ \\ | |/ _` |" ++ res
    putStrLn $ green ++ b ++ " |  _| | | | | | (_| (_| | " ++ yellow ++ "/ ___ \\ (_| | |  | | (_| (_) || | (_| |" ++ res
    putStrLn $ green ++ b ++ " |_|   |_|_| |_|\\___\\__,_| " ++ yellow ++ "/_/   \\_\\__, |_|  |_|\\___\\___/ |_|\\__,_|" ++ res
    putStrLn $ yellow ++ "                                       |___/                         " ++ res
    putStrLn $ white ++ i ++ "           Sistema de Gestión de Operaciones Agrícolas v1.5" ++ res
