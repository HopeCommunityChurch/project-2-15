# Some Random Types
They'll be written in a purescript like language (basically,
purescript, but probably won't compile). The main point is to get types together
that we can think and talk about.


## User
```purescript
newtype UserId = UserId UUID -- No idea what it'll actually be, probably just stick with UUID
  deriving (Eq, Ord, Hashable) -- Ord/Hashable for Sets and Maps

type User =
  { userId :: UserId
  , image :: Maybe Url -- Probably not added right away
  , name :: Text
  , email :: Text -- Hidden from other users
  , groups :: List GroupId
  }
```

## Group
```purescript
newtype GroupId = GroupId UUID
  deriving (Eq, Ord, Hashable)


type Group =
  { groupId :: GroupId
  , name :: Text
  , users :: List UserId
  , leaders :: List UserId
  , chuch :: ChurchId
  }
```

## Church
```purescript
newtype ChurchId = ChurchId UUID
  deriving (Eq, Ord, Hashable)


type Church =
  { churchId :: GroupId
  , name :: Text
  , elders :: List UserId
  }
```


## Something useful
```purescript
data UserAssignable
  = User UserId
  | Group GroupId
  | Church ChurchId
```


## Template Document
```purescript
newtype TempDocId = TempDocId UUID
  deriving (Eq, Ord, Hashable)

type TemplateDocument =
  { docId :: TempDocId
  , doc :: SomeThing -- The actual document
  }
```

## Study
```purescript
newtype StudyId = StudyId UUID
  deriving (Eq, Ord, Hashable)

type Study =
  { studyId :: StudyId
  , owner :: UserAssignable
  , for :: UserAssignable
  , name :: Text
  , templateDocument :: Maybe TempDocId
  }
```


## Document
```purescript
newtype DocId = DocId UUID
  deriving (Eq, Ord, Hashable)

type Document =
  { docId :: DocId
  , name :: Text -- Might not need this since the study has the name too
  , doc :: SomeThing -- The actual document
  , study :: StudyId
  , viewers :: List UserAssignable
  , editors :: List UserAssignable
  }
```
