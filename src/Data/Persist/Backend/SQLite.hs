{-# LANGUAGE TypeSynonymInstances #-}
module Data.Persist.Backend.SQLite (SQLite, runSQLite) where

import Database.HDBC (quickQuery', fromSql, toSql, SqlValue (..), commit, disconnect)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Data.Persist.Backend.Interface
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.ByteString.Char8 (unpack)

data SQLiteState = SQLiteState {
  conn :: Connection,
  debug_ :: String -> SQLite ()
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
debug s = sqliteLift (gets debug_) >>= \x -> x s

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

  updateImpl x tableName record = do
    c <- sqliteLift (gets conn)
    let (keys, bindVals) = unzip record
        query            = updateSQL tableName keys x
    debug query
    liftIO $ quickQuery' c query (map toSqlValue bindVals)
    return ()
      
  
  findRelationImpl key tableName = do
    c <- sqliteLift (gets conn)
    let query = findSQL tableName (Just $ either (const "x") (const "y") key) [either (const "y") (const "x") key]
    debug query
    result <- liftIO $ quickQuery' c query [toSql $ either id id key]
    return $ map (fromSql . head) $ result
  findImpl x tableName keys = do
    c <- sqliteLift (gets conn)
    let query = findSQL tableName (Just "id") keys
    debug query
    result <- liftIO $ quickQuery' c query  [toSql x]
    return $ fmap (map toDBValue) $ listToMaybe result

  findAllImpl tableName keys = do
    c <- sqliteLift (gets conn)
    let query = findSQL tableName Nothing ("id":keys)
    debug query
    result <- liftIO $ quickQuery' c query []
    debug (show result)
    return $ map (map toDBValue) $ result

    
  createSchemaForEntity tableName keys = do
    c <- sqliteLift (gets conn)
    let query = (createSQL tableName keys)
    debug query
    liftIO $ quickQuery' c query []
    return ()
  
  createSchemaForRelationship tableName = do
    c <- sqliteLift (gets conn)
    let query = (createSQL tableName ["x","y"])
    debug query
    liftIO $ quickQuery' c query []
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
  (result,_) <- runStateT (unSQLite operation) (SQLiteState c (liftIO . putStrLn))
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

findSQL :: String -> Maybe String -> [String] -> String
findSQL tableName whereColumn keys = unwords
 [ "SELECT"
 , commaList keys
 , "FROM"
 , tableName
 , whereClause
 ] where whereClause = maybe "" (\r -> "WHERE " ++ r ++ " = ?") whereColumn

updateSQL :: String -> [String] -> Int -> String
updateSQL tableName keys rowId = unwords
  [ "UPDATE"
  , tableName
  , "SET"
  , commaList $ map (\key -> key ++ " = ?")  keys
  , "WHERE id ="
  , show rowId
  ]


createSQL :: String -> [String] -> String
createSQL tableName keys = unwords ["CREATE TABLE", tableName, parens (commaList ("id INTEGER PRIMARY KEY":keys))]

tableSqlValues :: [(String, DBValue)] -> [SqlValue]
tableSqlValues = map (toSqlValue . snd)

toSqlValue :: DBValue -> SqlValue
toSqlValue (DBString s) = toSql s
toSqlValue (DBInt    s) = toSql s
toSqlValue (DBBool   s) = toSql s

-- Utilities
parens :: String -> String
parens x = "(" ++ x ++ ")"

commaList :: [String] -> String
commaList = intercalate ","
