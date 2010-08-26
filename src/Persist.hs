{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.IO (stdout, stderr, Handle, hPutStrLn, openFile, hClose, IOMode (..))
import Data.Persist.Parser
-- import Data.Persist.Pretty 
import Data.Persist.TypeCheck
import Data.Persist.Compile
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Pretty (prettyPrint)

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
  if not exists then putStrError ("File does not exist: " ++ filename opts) else do
    handle <- maybe (return stdout) (flip openFile WriteMode) (output opts)
    parseAndPrettyPrint handle (filename opts)
    hClose handle

parseAndPrettyPrint :: Handle -> String -> IO ()
parseAndPrettyPrint handle fp = do
  decls <- parseFile fp
  case decls of
       (ParseOk d) -> case typecheck d of
                           Left err -> putStrError $ "typecheck error: " ++ err
                           Right () -> hPutStrLn handle $ prettyPrint $ compile d
       (ParseFailed _ e) -> putStrError "Parse error:" >> putStrError e

putStrError = hPutStrLn stderr

test = parseAndPrettyPrint stdout "examples/Quiz.phs"
