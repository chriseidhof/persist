{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Syntax

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
  decls   <- fmap (groupDecls . lines) (readFile (filename opts))
  mapM_ print (map parse decls)

groupDecls :: [String] -> [[String]]
groupDecls = filter (not . null) . run []
 where run :: [String] -> [String] -> [[String]]
       run i [] = [i]
       run i (x:xs) | all isSpace x   = i:(run [] xs)
                    | otherwise       = run (i ++ [x]) xs

data Relationship = Relationship String
 deriving (Show)

parse :: [String] -> ParseResult (Either Decl Relationship)
parse decl@(x:_) = case head (words x) of
                     "data"         -> fmap Left $ parseDecl (unlines decl)
                     "relationship" -> ParseOk (Right $ Relationship $ unlines decl)
                     o              -> ParseFailed (SrcLoc "" 0 0) ("Not recognized: " ++ o)
