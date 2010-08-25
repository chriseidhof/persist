module Data.Persist.Parser (parseFile) where

import Data.Persist.AST
import Data.Char (isSpace)
import Language.Haskell.Exts.Parser hiding (parse)
import Language.Haskell.Exts.Syntax

parseFile :: String -> IO (ParseResult [Either Decl Relationship])
parseFile filename = fmap pipeline (readFile filename)


pipeline :: String -> ParseResult [Either Decl Relationship]
pipeline = sequence . map parse . groupDecls . lines

groupDecls :: [String] -> [[String]]
groupDecls = filter (not . null) . run []
 where run :: [String] -> [String] -> [[String]]
       run i [] = [i]
       run i (x:xs) | all isSpace x   = i:(run [] xs)
                    | otherwise       = run (i ++ [x]) xs

parse :: [String] -> ParseResult (Either Decl Relationship)
parse []         = error "parse: internal consistency error."
parse decl@(x:_) = case head (words x) of
                     "data"          -> fmap Left  $ parseDecl (unlines decl)
                     "relationship"  -> fmap Right $ parseRelationship (words $ unlines decl)
                     o               -> ParseFailed (SrcLoc "" 0 0) ("Not recognized: " ++ o)

parseRelationship :: [String] -> ParseResult Relationship
parseRelationship ["relationship", nm, "=", ident1, multiplicity, ident2] = do
  m <- parseMultiplicity multiplicity
  return (Relationship nm m ident1 ident2)
parseRelationship r = ParseFailed (SrcLoc "" 0 0) ("Not recognized: " ++ unwords r)


parseMultiplicity :: String -> ParseResult Multiplicity
parseMultiplicity "<1-1>" = ParseOk OneToOne
parseMultiplicity "<1-*>" = ParseOk OneToMany
parseMultiplicity "<*-1>" = ParseOk ManyToOne
parseMultiplicity "<*-*>" = ParseOk ManyToMany
parseMultiplicity m       = ParseFailed (SrcLoc "" 0 0) ("Multiplicity not recognized: " ++ m)
