{-# LANGUAGE MultiParamTypeClasses #-}
module Component where

import Data.Text (Text)
import qualified Data.Text as DT

import Types

class Component c where
  start :: c -> IO ()
  stop :: c -> IO ()

class Component c => Constructable c cfg err where
  construct :: cfg -> IO (Either err c)


data TaskStoreError = NoSuchTask TaskID
                    | Disconnected Text
                    | UnexpectedError Text deriving (Show)

class (Component c) => TaskStore c where
  persistTask :: c -> Validated (FullySpecifiedTask state) -> IO (Either TaskStoreError (FullySpecifiedTask state))
  completeTask :: c -> TaskID -> IO (Either TaskStoreError CompletedTask)
  getTask :: c -> TaskID -> IO (Either TaskStoreError (FullySpecifiedTask state))
  updateTask :: c -> PartialTask state -> IO (Either TaskStoreError (FullySpecifiedTask state))
  deleteTask :: c -> TaskID -> IO (Either TaskStoreError (FullySpecifiedTask state))
