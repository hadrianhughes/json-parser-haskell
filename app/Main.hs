module Main where

import Options.Applicative
import Text.Pretty.Simple
import JsonParser

runOpts :: FilePath -> IO ()
runOpts fp = do
  json <- readFile fp
  let result = runParser jsonRoot json
  case result of
    Left e  -> putStrLn ("Parse failed: " <> show e)
    Right x -> pPrint x

main :: IO ()
main = runOpts =<< execParser (filePath `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    filePath = strArgument (help "JSON file" <> metavar "FILE")
    infoString = "Run the JSON parser on the specified file."
