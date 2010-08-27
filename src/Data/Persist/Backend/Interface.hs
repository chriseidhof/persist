-- | Backends can implement the @Persistent@ class. Note that the code is untyped, to allow flexibility. Users of the library should never have to deal with this directly.
module Data.Persist.Backend.Interface 
  ( DBValue (..)
  , Persistent (..)
  ) where

-- | A database value is always one of these types.
data DBValue = DBString  String -- ^ A String
             | DBInt     Int    -- ^ An integer
             | DBBool    Bool   -- ^ A boolean
             deriving (Show, Read)


-- | This is the class backends need to implement
class (Monad p, Functor p) => Persistent p where
  createImpl       :: String              -- ^ The tableName
                   -> [(String,DBValue)]  -- ^ Keys and values
                   -> p Int               -- ^ The resulting id
  addRelationImpl :: Int    -- ^ The id of the first entity
                  -> Int    -- ^ The id of the second entity
                  -> String -- ^ The tableName
                  -> p () 
  findRelationImpl :: Either Int Int -- ^ @Left@ means the left side of the relation, @Right@ the right side.
                   -> String  -- ^ The tableName
                   -> p [Int] -- ^ The result is a list of matching ids
  findImpl :: Int      -- ^ The id of the record
           -> String   -- ^ The tableName
           -> [String] -- ^ A list of fields that need to be fetch
           -> p (Maybe [DBValue]) -- ^ If the record is found, a @Just@ containing the values for the fields