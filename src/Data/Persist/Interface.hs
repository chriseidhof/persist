{-# LANGUAGE FlexibleContexts, TypeOperators, FlexibleInstances, ScopedTypeVariables #-}
module Data.Persist.Interface 
  (  
  -- * Operations on relationships
    addRelation
  , findAllRelated
  , findAllRelated'
  , findRelation  
  , findRelation'  
  -- * Operations on entities
  , find
  , update
  , findAll
  -- * Functions exposed for auto-generated code
  , createSchemaEntity_  
  , createSchemaRelationship_ 
  , Ref (..)
  , create_
  , Relation (..)
  , Persistent (..)
  ) where

import Data.Maybe (catMaybes)
import Control.Monad (join)
import Control.Applicative
import Generics.Regular
import Data.Persist.Backend.Interface

-- | A reference to an entity of type @a@. Never construct these values manually: the implementation is exposed so that new backends can make use of it.
data Ref a = Ref {refKey :: Int}
 deriving Show

-- | Builds an `undefined` value of type @a@. Necessary for the generic functions.
getUndefined :: (Ref a) -> a
getUndefined _ = error "Trying to access getUndefined's value"

-- | Describes a relation between @a@ and @b@. Never construct these values manually.
data Relation a b = Relation { relTableName :: String }

-- | Creates a new value. To assure your database stays correct, never use this function directly.
create_ :: (Regular a, DatabaseRepr (PF a), Persistent p) => a -> p (Ref a)
create_ x = fmap Ref $ createImpl (tableName genX) (toDatabaseValue genX)
 where genX = from x

-- | Add a relation between entities of type @a@ and @b@.
addRelation :: Persistent p => Ref a -> Ref b -> Relation a b -> p ()
addRelation (Ref a) (Ref b) (Relation r) = addRelationImpl a b r

-- | Find all entities in the relationship
findAllRelated :: (Regular b, DatabaseRepr (PF b), Persistent p) => Ref a -> Relation a b -> p [b]
findAllRelated ref relation = findRelation ref relation >>= fmap catMaybes . mapM find 

-- | Find all entities in the relationship
findAllRelated' :: (Regular a, DatabaseRepr (PF a), Persistent p) => Ref b -> Relation a b -> p [a]
findAllRelated' ref relation = findRelation' ref relation >>= fmap catMaybes . mapM find 

findRelation :: Persistent p => Ref a -> Relation a b -> p [Ref b]
findRelation (Ref x) relation = fmap (map Ref) $ findRelationImpl (Left x) (relTableName relation)

findRelation' :: Persistent p => Ref b -> Relation a b -> p [Ref a]
findRelation' (Ref y) relation = fmap (map Ref) $ findRelationImpl (Right y) (relTableName relation)


-- | Find all entities
findAll :: forall p a . (Regular a, DatabaseRepr (PF a), Persistent p) => p [(Ref a, a)]
findAll = do
  if False then (return [(Ref 0, x)]) else do
    fmap (catMaybes . map convert) $ findAllImpl (tableName genX) (keys x)
 
 where x = (undefined :: a)
       genX = from x
       convert :: (Regular a, DatabaseRepr (PF a)) => [DBValue] -> Maybe (Ref a, a)
       convert (y:ys) = do ref <- Ref <$> dbValueAsInt y
                           res <- fromDatabaseValue ys
                           return (ref, to res)
       convert _      = Nothing

-- | Create the schema for entities of type @a@. The argument may be undefined (it's only necessary for the type)
createSchemaEntity_ :: (Regular a, DatabaseRepr (PF a), Persistent p) => a -> p ()
createSchemaEntity_ undefinedValue = createSchemaForEntity (tableName $ from undefinedValue) (keys undefinedValue)

-- | Create the schema for the relationship
createSchemaRelationship_ :: Persistent p => Relation a b -> p ()
createSchemaRelationship_ rel = createSchemaForRelationship (relTableName rel)

update :: (Regular a, DatabaseRepr (PF a), Persistent p) => Ref a -> a -> p ()
update (Ref x) value = updateImpl x (tableName genericValue) (toDatabaseValue genericValue)
 where genericValue = from value

-- | Finds an entity by reference.
find :: (Regular a, DatabaseRepr (PF a), Persistent p) => Ref a -> p (Maybe a)
find r@(Ref x) = fmap (join . fmap (fmap to . fromDatabaseValue)) $ findImpl x (tableName $ from undefinedValue) (keys undefinedValue)
 where undefinedValue = getUndefined r
   -- todo rewrite, join should be unnecessary
   

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
