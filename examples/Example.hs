module Example where

import Model
import Data.Persist.Interface
import Data.Persist.Backend.SQLite
import System.Directory (removeFile, doesFileExist)
import Control.Monad.Trans (liftIO)
import Control.Monad (when)

test :: IO ()
test = removeFileIfExists "test.sqlite3" >> (runSQLite "test.sqlite3" $ do
  -- Create the schema for our database
  createSchema

  -- Create a new Quiz entity
  quizRef     <- createQuiz (Quiz "my description" "my title")
  -- Add two questions to the quiz
  createQuestion (Question "Favorite color?" "red" "green" "blue")         quizRef
  createQuestion (Question "Favorite city?" "Utrecht" "Berlin" "Denekamp") quizRef
  -- Find the newly created quiz entity
  dbQuiz      <- find quizRef
  liftIO $ print dbQuiz

  -- Update the quiz entity
  update quizRef (Quiz "new description" "new title")

  -- Find the quiz again, and print the new value
  dbQuiz'     <- find quizRef
  liftIO $ print dbQuiz'


  -- Find the related questions
  questions'  <- findAllRelated quizRef questions
  liftIO $ mapM_ print questions'

  -- Find all questions
  questions'' <- findAll :: SQLite [(Ref Question, Question)]
  liftIO $ mapM_ print questions''

  )

removeFileIfExists :: String -> IO ()
removeFileIfExists s = do
  exists <- doesFileExist s
  when exists (removeFile s)
