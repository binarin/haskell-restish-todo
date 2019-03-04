{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import GHC.TypeLits
import           Data.Int (Int64)
import Data.UUID
import Data.Maybe (isNothing)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Functor.Identity


data {- kind -} TaskState = NotStarted
                          | InProgress
                          | Finished deriving (Enum, Read, Show)

type family ShowTaskState (a :: TaskState) :: Symbol where
  ShowTaskState NotStarted = "not_started"
  ShowTaskState InProgress = "in_progress"
  ShowTaskState Finished = "finished"

newtype TaskName = TaskName { getTName :: Text } deriving (Eq, Show)
newtype TaskDesc = TaskDesc { getTDesc :: Text } deriving (Eq, Show)

data Task token (state :: TaskState) = Task { tName :: Apply token TaskName
                                            , tDescription :: Apply token TaskDesc
                                            }


type family Apply (token :: *) (field :: *)

data MaybeToken
type instance Apply MaybeToken f = Maybe f

data IdToken
type instance Apply IdToken f = f

type Complete f = f IdToken
type Partial f = f MaybeToken

type CompletedTask = Task IdToken 'Finished
deriving instance Eq CompletedTask
deriving instance Show CompletedTask

type IncompletePartialTask = Task MaybeToken 'InProgress
deriving instance Eq IncompletePartialTask
deriving instance Show IncompletePartialTask

type IncompleteTask = Task IdToken 'InProgress
deriving instance Eq IncompleteTask
deriving instance Show IncompleteTask

type NotStartedPartialTask = Task MaybeToken 'NotStarted
deriving instance Eq NotStartedPartialTask
deriving instance Show NotStartedPartialTask

type NotStartedTask = Task IdToken 'NotStarted
deriving instance Eq NotStartedTask
deriving instance Show NotStartedTask

type FullySpecifiedTask state = Task IdToken state
type PartialTask state = Task MaybeToken state

newtype FieldName = FieldName { getFieldName :: Text } deriving (Eq, Show, Read)

data ValidationError = InvalidField FieldName
                     | MissingField FieldName
                     | InternalError deriving (Eq, Show, Read)

newtype Validated t = Validated { getValidatedObj :: t }

class Validatable t where
  validate :: t -> Either ValidationError (Validated t)

instance Validatable (Task IdToken a) where
  validate t = do
    when (isEmpty nm) $ Left (InvalidField (FieldName "name"))
    when (isEmpty dsc) $ Left (InvalidField (FieldName "description"))
    pure $ Validated t

    where
      isEmpty txt = DT.length txt == 0

      nm = getTName (tName t)
      dsc = getTDesc (tDescription t)

instance Validatable (Task MaybeToken a) where
  validate t = do
    when (isNothing $ tName t) $ Left (MissingField (FieldName "name"))
    when (isNothing $ tDescription t) $ Left (MissingField (FieldName "name"))
    case Task <$> tName t <*> tDescription t :: Maybe (Task IdToken a) of
      Nothing -> Left InternalError
      Just _ -> pure $ Validated t

newtype TaskID = TaskID { getTaskID :: Int } deriving (Eq, Show, Read)

data WithID a = UUIDID UUID a
              | Int64ID Int64 a
