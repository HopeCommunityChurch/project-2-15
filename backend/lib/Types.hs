module Types (NewType (..), UserId, Email, GroupId, ChurchId, StudyId, DocId) where

import Data.UUID (UUID)
import Data.Hashable (Hashable)


newtype NewType p a = MkNewType a
  deriving (Generic)
  deriving newtype (Show, Read, Hashable, Eq, Ord)

instance Wrapped (NewType p a)


data UserId'
type UserId = NewType UserId' UUID

data Email'
type Email = NewType Email' Text


data GroupId'
type GroupId = NewType GroupId' UUID

data ChurchId'
type ChurchId = NewType ChurchId' UUID


data StudyId'
type StudyId = NewType StudyId' UUID


data DocId'
type DocId = NewType DocId' UUID
