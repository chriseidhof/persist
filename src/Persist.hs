{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory (doesFileExist)
import Data.Persist.Parser

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
    mapM_ print decls
