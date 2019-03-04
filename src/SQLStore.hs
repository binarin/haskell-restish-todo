{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
module SQLStore where

import Data.Proxy
import Control.Monad (when)
import Control.Exception (throw)
import qualified Data.Text as T
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.SQLite.Simple (Connection, ToRow(..), FromRow(..), SQLData(..), field)
import           Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed))
import           Database.SQLite.Simple.ToField (ToField(..))
import           Database.SQLite.Simple.FromRow (RowParser)
import GHC.TypeLits (KnownSymbol, symbolVal)

import           Config
import           Types
import           Component

instance ToField TaskName where
  toField (TaskName n) = SQLText n

instance ToField TaskDesc where
  toField (TaskDesc d) = SQLText d

instance ToRow a => ToRow (Validated a) where
  toRow = toRow . getValidatedObj

instance ToField UUID where
  toField = SQLText . UUID.toText

instance ToField TaskState where
  toField = SQLText . T.pack . show

instance ToRow a => ToRow (WithID a) where
  toRow (UUIDID i a) = [toField i] <> toRow a
  toRow (Int64ID i a) = [toField i] <> toRow a

taskStateName :: forall state. KnownSymbol (ShowTaskState state) => Proxy (state :: TaskState) -> String
taskStateName _ = symbolVal (Proxy :: Proxy (ShowTaskState state))

instance forall state.  KnownSymbol (ShowTaskState state) => ToRow (FullySpecifiedTask (state :: TaskState)) where
  toRow Task { tName, tDescription } = toRow (tName, tDescription, T.pack (taskStateName (Proxy :: Proxy state)))

instance FromRow a => FromRow (WithID a) where
  fromRow = do
    (field >>= makeCtr) <*> fromRow

    where
      makeCtr :: SQLData -> RowParser (a -> WithID a)
      makeCtr (SQLText t) = case UUID.fromText t of
                              Nothing -> throw (ConversionFailed (show t) "Text" "Invaild uuid")
                              Just u -> pure $ UUIDID u
      makeCtr (SQLInteger i) = pure $ Int64ID i
      makeCtr sqldata = throw (ConversionFailed (show sqldata) "???" "Unexpected sql data type")

-- deriving instance FromField TaskName
-- deriving instance FromField TaskDesc

instance forall state token. ( KnownSymbol (ShowTaskState state)
                             , FromField (Apply token TaskName)
                             , FromField (Apply token TaskDesc)
                             ) => FromRow (Task token (state :: TaskState)) where
  fromRow = do
    nm <- field
    dsc <- field
    tp <- field
    let expectedTp = T.pack $ taskStateName (Proxy :: Proxy state)
    when (tp /= expectedTp) $ throw (ConversionFailed (show tp) "???" ("Expected state: " ++ show expectedTp))
    pure $ Task nm dsc


data SQLiteTaskStore = SQLiteTaskStore { stsCfg :: TaskStoreConfig 'Final
                                       , stsConn :: Maybe Connection
                                       }

instance Component SQLiteTaskStore where
  start = undefined
  stop = undefined

instance Constructable SQLiteTaskStore (TaskStoreConfig 'Final) TaskStoreError where
  construct = undefined

instance TaskStore SQLiteTaskStore where
  persistTask = undefined
  completeTask = undefined
  getTask = undefined
  updateTask = undefined
  deleteTask = undefined
