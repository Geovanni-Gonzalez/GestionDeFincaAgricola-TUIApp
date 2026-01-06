module Files where

import Types
import System.Directory (doesFileExist)
import System.IO

-- | Manual split if we don't want to add dependency
splitComma :: String -> [String]
splitComma s = case dropWhile (==',') s of
                "" -> []
                s' -> w : splitComma s''
                      where (w, s'') = break (==',') s'

-- | Load tools from a CSV file (append logic in Menu, this just reads)
loadToolsFromCSV :: FilePath -> IO [Tool]
loadToolsFromCSV path = do
    exists <- doesFileExist path
    if not exists
        then return []
        else do
            contents <- readFile path
            let linesOfFile = lines contents
            return $ map parseTool linesOfFile
  where
    parseTool line = let parts = splitComma line in
        Tool (parts !! 0) (parts !! 1) (parts !! 2) (readToolType (parts !! 3))
    readToolType t
        | t == "manual" = Manual
        | t == "motorizada" = Motorizada
        | t == "automatizada" = Automatizada
        | otherwise = Manual

-- | Global State Persistence
statePath :: FilePath
statePath = "farm_data.txt"

saveState :: AppState -> IO ()
saveState state = writeFile statePath (show state)

loadState :: IO AppState
loadState = do
    exists <- doesFileExist statePath
    if not exists
        then return initialState
        else do
            contents <- readFile statePath
            return $ read contents
