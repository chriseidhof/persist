{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory (doesFileExist)
import Data.Persist.Parser
import Data.Persist.Pretty 
import Data.Persist.TypeCheck
import Data.Persist.Compile
import Language.Haskell.Exts.Parser hiding (parse)

data Persist = Persist {
  filename :: String,
  output   :: Maybe String
} deriving (Show, Data, Typeable)


persist :: Persist
persist = Persist {
  filename = def &= help "Input file"  &= typFile &= argPos 0,
  output   = def &= help "Output file" &= typFile
} &= summary "This program is the command line interface to the Haskell persist library"

main :: IO ()
main =  do
  opts    <- cmdArgs persist
  exists  <- doesFileExist (filename opts)
  if not exists then putStrLn ("File does not exist: " ++ filename opts) else do
    decls <- parseFile (filename opts)
    print decls

test :: IO ()
test = do
  decls <- parseFile "examples/Quiz.phs"
  case decls of
       (ParseOk d) -> case typecheck d of
                           Left err -> putStrLn $ "typecheck error: " ++ err
                           Right () -> putStrLn $ unlines $ map pretty $ compile d
       (ParseFailed _ e) -> putStrLn "Parse error:" >> putStrLn e
