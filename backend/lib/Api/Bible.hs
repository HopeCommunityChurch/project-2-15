module Api.Bible where

import Api.Auth (AuthUser)
import Api.Errors qualified as Errs
import DbHelper (MonadDb)
import Servant
import Network.Wreq qualified as Wreq
import GHC.Records (HasField)
import Text.Parsec qualified as Parsec
import Bible.Esv.Parser qualified as ESV


newtype ESVEnv = MkESVEnv ByteString

type HasESVEnv env = HasField "esvToken" env ESVEnv

-- const url = `https://api.esv.org/v3/passage/text/?q=${encodeURIComponent(
--   query
-- )}&include-passage-references=false&include-verse-numbers=true&include-footnotes=false&include-footnote-body=false&include-headings=false&include-short-copyright=false&include-copyright=false&indent-paragraphs=0&indent-poetry=false&line-length=0`;

data ApiESVResponse = MkApiESVResponse
  { canonical :: Text
  , passages :: [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


getVersesApi
  :: ( MonadIO m
     , MonadReader env m
     , HasESVEnv env
     )
  => Text
  -> m ApiESVResponse
getVersesApi query = do
  (MkESVEnv esvToken) <- asks (.esvToken)
  let opts = Wreq.defaults
              & Wreq.param "include-passage-references" .~ ["false"]
              & Wreq.param "include-verse-numbers" .~ ["true"]
              & Wreq.param "include-footnotes" .~ ["false"]
              & Wreq.param "include-footnote-body" .~ ["false"]
              & Wreq.param "include-headings" .~ ["false"]
              & Wreq.param "include-short-copyright" .~ ["false"]
              & Wreq.param "include-copyright" .~ ["false"]
              & Wreq.param "indent-paragraphs" .~ ["0"]
              & Wreq.param "indent-poetry" .~ ["false"]
              & Wreq.param "line-length" .~ ["0"]
              & Wreq.param "q" .~ [query]
              & Wreq.header "Authorization" .~ ["Token " <> esvToken]
  resp <- liftIO $ Wreq.asJSON =<< Wreq.getWith opts "https://api.esv.org/v3/passage/text/"
  pure (resp ^. Wreq.responseBody :: ApiESVResponse)


data ESVResponse = MkESVResponse
  { canonical :: Text
  , passage :: [ESV.Verse]
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


data VersesNotFound = VersesNotFound
  deriving (Show, Generic)
  deriving anyclass (Exception, ToJSON, Errs.ApiException)

data ParseError = ParseError Text
  deriving (Show, Generic)
  deriving anyclass (Exception, ToJSON, Errs.ApiException)


getVerses
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasESVEnv env
     )
  => AuthUser
  -> Text
  -> m ESVResponse
getVerses _ query = do
  result <- getVersesApi query
  when (result.canonical == "") $ Errs.throwApi VersesNotFound
  let eBibleRef = Parsec.parse ESV.bibleRefParser "" result.canonical
  case eBibleRef of
    Left err -> Errs.throwApi $ ParseError (show err)
    Right bibleRef -> do
      let state = ESV.MkPState
                    bibleRef.book
                    bibleRef.verses.chapterStart
                    bibleRef.verses.verseStart
      let epassages =
            Parsec.runParser ESV.passagesParser state "" (fold result.passages)
      case epassages of
        Left err -> Errs.throwApi $ ParseError (show err)
        Right passages ->
          pure $ MkESVResponse result.canonical passages


type Api =
  AuthProtect "cookie"
    :> "esv"
    :> Summary "Gets ESV verses"
    :> Description "Gets ESV verses"
    :> QueryParam' [Required, Strict] "q" Text
    :> Get '[JSON] ESVResponse


server
  :: MonadDb env m
  => HasESVEnv env
  => ServerT Api m
server =
  getVerses

