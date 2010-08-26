{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances, ExplicitForAll #-}
module Data.Persist.Backend.Interface 
  ( Ref (..)
  , DBValue (..)
  , create_
  , addRelation
  , find
  , Persistent (..)
  , Relation (..)
  ) where

import Control.Applicative
import Generics.Regular
import Control.Monad (join)

--todo: also index ref by persistent class
data Ref a = Ref {refKey :: Int}
 deriving Show

getUndefined :: (Ref a) -> a
getUndefined _ = error "Trying to access getUndefined's value"

data Relation a b = Relation { relTableName :: String }

-- | A database value is always one of these types.
data DBValue = DBString  String
             | DBInt     Int
             | DBBool    Bool
             deriving (Show, Read)


create_ :: (Regular a, DatabaseRepr (PF a), Persistent p) => a -> p (Ref a)
create_ x = fmap Ref $ createImpl (tableName genX) (toDatabaseValue genX)
 where genX = from x

addRelation :: Persistent p => Ref a -> Ref b -> Relation a b -> p ()
addRelation (Ref a) (Ref b) (Relation r) = addRelationImpl a b r

find :: forall a p . (Regular a, DatabaseRepr (PF a), Persistent p) => Ref a -> p (Maybe a)
find r@(Ref x) = fmap (join . fmap (fmap to . fromDatabaseValue)) $ findImpl x (tableName $ from undefinedValue) (keys undefinedValue)
 where undefinedValue = getUndefined r
-- todo rewrite, join should be unnecessary

-- | This is the class backends need to implement.
class (Monad p, Functor p) => Persistent p where
  createImpl       :: String              -- | The tableName
                   -> [(String,DBValue)]  -- | Keys and values
                   -> p Int               -- | The resulting id
  addRelationImpl :: Int
                  -> Int
                  -> String
                  -> p ()
  findImpl :: Int -> String -> [String] -> p (Maybe [DBValue])



-- Conversion code

keys :: (Regular a, DatabaseValue (PF a)) => a -> [String]
keys = map fst . toDatabaseValue . from

class DatabaseValue f => DatabaseRepr f where
  tableName              :: f a -> String

class DatabaseField f where
  toField   :: f a -> DBValue
  fromField :: DBValue -> Maybe (f a)

class DatabaseValue f where
  toDatabaseValue   :: f a -> [(String, DBValue)]
  fromDatabaseValue :: [DBValue] -> Maybe (f a)

instance (DatabaseValue f) => DatabaseValue (C c f) where
  toDatabaseValue ~(C x) = toDatabaseValue x
  fromDatabaseValue x   = C <$> fromDatabaseValue x

instance (DatabaseValue f, DatabaseValue g) => DatabaseValue (f :*: g) where
  toDatabaseValue ~(x :*: y) = toDatabaseValue x ++ toDatabaseValue y
  fromDatabaseValue (x:xs)  = (:*:) <$> fromDatabaseValue [x] <*> fromDatabaseValue xs
  fromDatabaseValue _       = Nothing

instance (Selector s, DatabaseField f) => DatabaseValue (S s f) where
  toDatabaseValue ~s@(S x) = [(selName s, toField x)]
  fromDatabaseValue [x]   = S <$> fromField x
  fromDatabaseValue _     = Nothing

instance DatabaseField (K String) where
  toField ~(K x)          = DBString x
  fromField (DBString x) = Just (K x)
  fromField _            = Nothing

instance DatabaseField (K [Int]) where
  toField ~(K x) = DBString $ show x
  fromField (DBString x) = Just (K $ read x)
  fromField _            = Nothing

instance (Constructor c, DatabaseValue f) => DatabaseRepr (C c f) where
  tableName           x = conName x
