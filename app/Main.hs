module Main where

import Options.Applicative
import JsonParser

runOpts :: FilePath -> IO ()
runOpts fp = do
  json <- readFile fp
  let result = runParser jsonRoot json
  case result of
    Nothing -> putStrLn "Parse failed."
    Just x  -> putStrLn (show x)

main :: IO ()
main = runOpts =<< execParser (filePath `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    filePath = strArgument (help "JSON file" <> metavar "FILE")
    infoString = "Run the JSON parser on the specified file."
