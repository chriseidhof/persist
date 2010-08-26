{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies #-}
module Data.Persist.Backend.SQLite where

import Generics.Regular
import Database.HDBC (quickQuery', fromSql, toSql, SqlValue (..), commit, disconnect)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Data.Persist.Backend.Interface
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)

-- Examples
data Response = Response {
  name    :: String,
  email   :: String,
  date    :: String,
  answers :: [Int]
}

example :: Response
example = Response "chris" "test" "date" [1,2,3]

$(deriveAll ''Response "PFResponse")
type instance PF Response = PFResponse

data SQLiteState = SQLiteState {
  conn :: Connection
}

newtype SQLite a = SQLite {unSQLite :: StateT SQLiteState IO a}

instance Functor SQLite where
  fmap f (SQLite a) = SQLite $ fmap f a

instance Monad SQLite where
  return = SQLite . return
  (SQLite l) >>= r = SQLite $ l >>= (fmap unSQLite r)

instance MonadIO SQLite where
  liftIO = SQLite . liftIO

sqliteLift :: StateT SQLiteState IO a -> SQLite a
sqliteLift = SQLite

debug :: String -> SQLite ()
debug = liftIO . putStrLn

instance Persistent SQLite where
  createImpl tableName record = do
    c <- sqliteLift (gets conn)
    let query    = insertSQL tableName record
        bindVals = tableSqlValues record
    debug query
    liftIO $ quickQuery' c query bindVals

    [[rowId]] <- liftIO $ quickQuery' c "SELECT last_insert_rowid()" []
      
    return (fromSql rowId) -- TODO!
    
  addRelationImpl a b tableName = do
    c <- sqliteLift (gets conn)
    let record   = [("x", DBInt a), ("y", DBInt b)]
        query    = insertSQL tableName record
        bindVals = tableSqlValues record
    debug query
    liftIO $ quickQuery' c query bindVals
    return ()


runSQLite :: String -> SQLite a -> IO a
runSQLite dbName operation = do
  c <- connectSqlite3 dbName
  (result,_) <- runStateT (unSQLite operation) (SQLiteState c)
  commit c
  disconnect c
  return result

insertSQL :: String -> [(String,DBValue)] -> String
insertSQL nm keysAndValues = unwords
 [ "INSERT INTO "
 , nm
 , parens (commaList keys)
 , "VALUES"
 , parens (commaList $ map (const "?") keys)
 ]
 where (keys,_) = unzip keysAndValues

tableSqlValues :: [(String, DBValue)] -> [SqlValue]
tableSqlValues = map (toSqlValue . snd)
 where toSqlValue (DBString s) = toSql s
       toSqlValue (DBInt    s) = toSql s
       toSqlValue (DBBool   s) = toSql s

-- Utilities
parens :: String -> String
parens x = "(" ++ x ++ ")"

commaList :: [String] -> String
commaList = intercalate ","

lower :: String -> String
lower = map toLower
