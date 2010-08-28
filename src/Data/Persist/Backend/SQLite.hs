{-# LANGUAGE TypeSynonymInstances #-}
module Data.Persist.Backend.SQLite (SQLite, runSQLite) where

import Database.HDBC (quickQuery', fromSql, toSql, SqlValue (..), commit, disconnect)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Data.Persist.Backend.Interface
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.ByteString.Char8 (unpack)

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
      
    return (fromSql rowId)
    
  addRelationImpl a b tableName = do
    c <- sqliteLift (gets conn)
    let record   = [("x", DBInt a), ("y", DBInt b)]
        query    = insertSQL tableName record
        bindVals = tableSqlValues record
    debug query
    liftIO $ quickQuery' c query bindVals
    return ()
  
  findRelationImpl key tableName = do
    c <- sqliteLift (gets conn)
    let query = findSQL tableName (either (const "x") (const "y") key) [either (const "y") (const "x") key]
    result <- liftIO $ quickQuery' c query [toSql $ either id id key]
    return $ map (fromSql . head) $ result
  findImpl x tableName keys = do
    c <- sqliteLift (gets conn)
    result <- liftIO $ quickQuery' c (findSQL tableName "ROWID" keys) [toSql x]
    return $ fmap (map toDBValue) $ listToMaybe result
    
  createSchemaForEntity tableName keys = do
    c <- sqliteLift (gets conn)
    liftIO $ quickQuery' c (createSQL tableName keys) []
    return ()
  
  createSchemaForRelationship tableName = do
    c <- sqliteLift (gets conn)
    liftIO $ quickQuery' c (createSQL tableName ["x","y"]) []
    return ()

toDBValue :: SqlValue -> DBValue
toDBValue (SqlString s)     = DBString s
toDBValue (SqlByteString s) = DBString (unpack s)
toDBValue (SqlInt32 i)      = DBInt $ fromIntegral i
toDBValue (SqlInt64 i)      = DBInt $ fromIntegral i
toDBValue (SqlInteger i)    = DBInt (fromIntegral i)
toDBValue v                 = error $ "SQLlite.toDBValue Unsupported value: " ++ show v

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

findSQL :: String -> String -> [String] -> String
findSQL tableName column keys = unwords
 [ "SELECT"
 , commaList keys
 , "FROM"
 , tableName
 , "WHERE"
 , column  
 , "= ?"
 ]

createSQL :: String -> [String] -> String
createSQL tableName keys = unwords ["CREATE TABLE", tableName, parens (commaList keys)]

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