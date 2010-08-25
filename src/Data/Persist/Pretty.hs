{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.Persist.Pretty where

import Data.Persist.AST
import Language.Haskell.Exts.Syntax (Decl)
import qualified Language.Haskell.Exts.Pretty as P

class Pretty a where
  pretty :: a -> String

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left l)  = pretty l
  pretty (Right r) = pretty r

instance Pretty Decl where
  pretty = P.prettyPrint


--TODO: remove me
instance Pretty Relationship where
  pretty (Relationship nm multiplicity l r) = "-- " ++ unwords ["relationship: ", nm, show multiplicity, l, r]
