module Data.Persist.TypeCheck where

import Data.Persist.AST
import Data.Either (partitionEithers)
import Control.Monad.Error ()
import Language.Haskell.Exts.Syntax

type Err a = Either String a

typecheck :: [Either Decl Relationship] -> Err ()
typecheck schema = do let (decls, rels) = partitionEithers schema
                      checkNoRecursion decls
                      checkTypesAreBetweenDeclDatatypes decls rels
                      return ()

-- TODO: implement these:
--
checkNoRecursion :: [Decl] -> Err ()
checkNoRecursion _ = return ()

checkTypesAreBetweenDeclDatatypes :: [Decl] -> [Relationship] -> Err ()
checkTypesAreBetweenDeclDatatypes _ _ = return ()
