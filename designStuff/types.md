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



## Study Template
```purescript
newtype STId = STId UUID
  deriving (Eq, Ord, Hashable)

type ST =
  { stId :: STId
  , name :: Text
  , templateDocument :: SomeThing -- The actual document
  }
```

## Study
```purescript
newtype StudyId = StudyId UUID
  deriving (Eq, Ord, Hashable)

type Study =
  { studyId :: StudyId
  , participants :: List UserId
  , name :: Text
  , studyTemplateId :: STId
  , templateDocument :: SomeThing -- The actual document
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
  , editors :: List UserId
  }
```
