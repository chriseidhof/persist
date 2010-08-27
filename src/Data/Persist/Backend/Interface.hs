module Data.Persist.Backend.Interface 
  ( DBValue (..)
  , Persistent (..)
  ) where

-- | A database value is always one of these types.
data DBValue = DBString  String
             | DBInt     Int
             | DBBool    Bool
             deriving (Show, Read)


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
