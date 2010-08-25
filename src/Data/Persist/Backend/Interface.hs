module Data.Persist.Backend.Interface where

import Generics.Regular
import Data.Persist.AST

data Ref a = Ref Int

class Persistent p where
  create          :: (Regular record) 
                  => record 
                  -> p (Ref record)
  addRelation :: (Regular from, Regular to) 
              => Ref from 
              -> Ref to
              -> Relationship
              -> p ()
