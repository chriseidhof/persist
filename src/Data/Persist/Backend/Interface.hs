{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances #-}
module Data.Persist.Backend.Interface 
  ( Ref (..)
  , DBValue (..)
  , create
  , Persistent (..)
  ) where

import Control.Applicative
import Generics.Regular
import Data.Persist.AST

--todo: also index ref by persistent class
data Ref a = Ref Int

-- | A database value is always one of these types.
data DBValue = DBString  String
             | DBInt     Int
             | DBBool    Bool
             deriving (Show, Read)


create :: (Regular a, DatabaseRepr (PF a), Persistent p) => a -> p (Ref a)
create x = fmap Ref $ createImpl (tableName genX) (toDatabaseValue genX)
 where genX = from x


class Functor p => Persistent p where
  createImpl       :: String              -- | The tableName
                   -> [(String,DBValue)]  -- | Keys and values
                   -> p Int               -- | The resulting id
  addRelationImpl :: Int
                  -> Int
                  -> Relationship
                  -> p ()



-- Conversion code

class DatabaseValue f => DatabaseRepr f where
  tableName              :: f a -> String

class DatabaseField f where
  toField   :: f a -> DBValue
  fromField :: DBValue -> Maybe (f a)

class DatabaseValue f where
  toDatabaseValue   :: f a -> [(String, DBValue)]
  fromDatabaseValue :: [DBValue] -> Maybe (f a)

instance (DatabaseValue f) => DatabaseValue (C c f) where
  toDatabaseValue (C x) = toDatabaseValue x
  fromDatabaseValue x   = C <$> fromDatabaseValue x

instance (DatabaseValue f, DatabaseValue g) => DatabaseValue (f :*: g) where
  toDatabaseValue (x :*: y) = toDatabaseValue x ++ toDatabaseValue y
  fromDatabaseValue (x:xs)  = (:*:) <$> fromDatabaseValue [x] <*> fromDatabaseValue xs
  fromDatabaseValue _       = Nothing

instance (Selector s, DatabaseField f) => DatabaseValue (S s f) where
  toDatabaseValue s@(S x) = [(selName s, toField x)]
  fromDatabaseValue [x]   = S <$> fromField x
  fromDatabaseValue _     = Nothing

instance DatabaseField (K String) where
  toField (K x)          = DBString x
  fromField (DBString x) = Just (K x)
  fromField _            = Nothing

instance DatabaseField (K [Int]) where
  toField (K x) = DBString $ show x
  fromField (DBString x) = Just (K $ read x)
  fromField _            = Nothing

instance (Constructor c, DatabaseValue f) => DatabaseRepr (C c f) where
  tableName           x = conName x
