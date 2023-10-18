module Bible.Esv.Parser where

import Prelude hiding ((<|>), try)
import Text.Parsec
import Data.Char (digitToInt)
import Data.Text qualified as T
import Data.Aeson (ToJSON(..), Value(..))
import Data.OpenApi qualified as OpenApi

data Book
  = Genesis
  | Exodus
  | Leviticus
  | Numbers
  | Deuteronomy
  | Joshua
  | Judges
  | Ruth
  | FstSamuel
  | SndSamuel
  | FstKings
  | SndKings
  | FstChronicles
  | SndChronicles
  | Ezra
  | Nehemiah
  | Esther
  | Job
  | Psalms
  | Proverbs
  | Ecclesiastes
  | SongOfSolomon
  | Isaiah
  | Jeremiah
  | Lamentations
  | Ezekiel
  | Daniel
  | Hosea
  | Joel
  | Amos
  | Obadiah
  | Jonah
  | Micah
  | Nahum
  | Habakkuk
  | Zephaniah
  | Haggai
  | Zechariah
  | Malachi
  | Matthew
  | Mark
  | Luke
  | John
  | Acts
  | Romans
  | FstCorinthians
  | SndCorinthians
  | Galatians
  | Ephesians
  | Philippians
  | Colossians
  | FstThessalonians
  | SndThessalonians
  | FstTimothy
  | SndTimothy
  | Titus
  | Philemon
  | Hebrews
  | James
  | FstPeter
  | SndPeter
  | FstJohn
  | SndJohn
  | TrdJohn
  | Jude
  | Revelation
  deriving (Show, Generic, Enum, Bounded)

bookToText :: Book -> Text
bookToText Genesis = "Genesis"
bookToText Exodus = "Exodus"
bookToText Leviticus = "Leviticus"
bookToText Numbers = "Numbers"
bookToText Deuteronomy = "Deuteronomy"
bookToText Joshua = "Joshua"
bookToText Judges = "Judges"
bookToText Ruth = "Ruth"
bookToText FstSamuel = "1 Samuel"
bookToText SndSamuel = "2 Samuel"
bookToText FstKings = "1 Kings"
bookToText SndKings = "2 Kings"
bookToText FstChronicles = "1 Chronicles"
bookToText SndChronicles = "2 Chronicles"
bookToText Ezra = "Ezra"
bookToText Nehemiah = "Nehemiah"
bookToText Esther = "Esther"
bookToText Job = "Job"
bookToText Psalms = "Psalms"
bookToText Proverbs = "Proverbs"
bookToText Ecclesiastes = "Ecclesiastes"
bookToText SongOfSolomon = "SongOfSolomon"
bookToText Isaiah = "Isaiah"
bookToText Jeremiah = "Jeremiah"
bookToText Lamentations = "Lamentations"
bookToText Ezekiel = "Ezekiel"
bookToText Daniel = "Daniel"
bookToText Hosea = "Hosea"
bookToText Joel = "Joel"
bookToText Amos = "Amos"
bookToText Obadiah = "Obadiah"
bookToText Jonah = "Jonah"
bookToText Micah = "Micah"
bookToText Nahum = "Nahum"
bookToText Habakkuk = "Habakkuk"
bookToText Zephaniah = "Zephaniah"
bookToText Haggai = "Haggai"
bookToText Zechariah = "Zechariah"
bookToText Malachi = "Malachi"
bookToText Matthew = "Matthew"
bookToText Mark = "Mark"
bookToText Luke = "Luke"
bookToText John = "John"
bookToText Acts = "Acts"
bookToText Romans = "Romans"
bookToText FstCorinthians = "1 Corinthians"
bookToText SndCorinthians = "2 Corinthians"
bookToText Galatians = "Galatians"
bookToText Ephesians = "Ephesians"
bookToText Philippians = "Philippians"
bookToText Colossians = "Colossians"
bookToText FstThessalonians = "1 Thessalonians"
bookToText SndThessalonians = "2 Thessalonians"
bookToText FstTimothy = "1 Timothy"
bookToText SndTimothy = "2 Timothy"
bookToText Titus = "Titus"
bookToText Philemon = "Philemon"
bookToText Hebrews = "Hebrews"
bookToText James = "James"
bookToText FstPeter = "1 Peter"
bookToText SndPeter = "2 Peter"
bookToText FstJohn = "1 John"
bookToText SndJohn = "2 John"
bookToText TrdJohn = "3 John"
bookToText Jude = "Jude"
bookToText Revelation = "Revelation"

textToBook :: Text -> Maybe Book
textToBook "Genesis" = Just Genesis
textToBook "Exodus" = Just Exodus
textToBook "Leviticus" = Just Leviticus
textToBook "Numbers" = Just Numbers
textToBook "Deuteronomy" = Just Deuteronomy
textToBook "Joshua" = Just Joshua
textToBook "Judges" = Just Judges
textToBook "Ruth" = Just Ruth
textToBook "1 Samuel" = Just FstSamuel
textToBook "2 Samuel" = Just SndSamuel
textToBook "1 Kings" = Just FstKings
textToBook "2 Kings" = Just SndKings
textToBook "1 Chronicles" = Just FstChronicles
textToBook "2 Chronicles" = Just SndChronicles
textToBook "Ezra" = Just Ezra
textToBook "Nehemiah" = Just Nehemiah
textToBook "Esther" = Just Esther
textToBook "Job" = Just Job
textToBook "Psalms" = Just Psalms
textToBook "Proverbs" = Just Proverbs
textToBook "Ecclesiastes" = Just Ecclesiastes
textToBook "SongOfSolomon" = Just SongOfSolomon
textToBook "Isaiah" = Just Isaiah
textToBook "Jeremiah" = Just Jeremiah
textToBook "Lamentations" = Just Lamentations
textToBook "Ezekiel" = Just Ezekiel
textToBook "Daniel" = Just Daniel
textToBook "Hosea" = Just Hosea
textToBook "Joel" = Just Joel
textToBook "Amos" = Just Amos
textToBook "Obadiah" = Just Obadiah
textToBook "Jonah" = Just Jonah
textToBook "Micah" = Just Micah
textToBook "Nahum" = Just Nahum
textToBook "Habakkuk" = Just Habakkuk
textToBook "Zephaniah" = Just Zephaniah
textToBook "Haggai" = Just Haggai
textToBook "Zechariah" = Just Zechariah
textToBook "Malachi" = Just Malachi
textToBook "Matthew" = Just Matthew
textToBook "Mark" = Just Mark
textToBook "Luke" = Just Luke
textToBook "John" = Just John
textToBook "Acts" = Just Acts
textToBook "Romans" = Just Romans
textToBook "1 Corinthians" = Just FstCorinthians
textToBook "2 Corinthians" = Just SndCorinthians
textToBook "Galatians" = Just Galatians
textToBook "Ephesians" = Just Ephesians
textToBook "Philippians" = Just Philippians
textToBook "Colossians" = Just Colossians
textToBook "1 Thessalonians" = Just FstThessalonians
textToBook "2 Thessalonians" = Just SndThessalonians
textToBook "1 Timothy" = Just FstTimothy
textToBook "2 Timothy" = Just SndTimothy
textToBook "Titus" = Just Titus
textToBook "Philemon" = Just Philemon
textToBook "Hebrews" = Just Hebrews
textToBook "James" = Just James
textToBook "1 Peter" = Just FstPeter
textToBook "2 Peter" = Just SndPeter
textToBook "1 John" = Just FstJohn
textToBook "2 John" = Just SndJohn
textToBook "3 John" = Just TrdJohn
textToBook "Jude" = Just Jude
textToBook "Revelation" = Just Revelation
textToBook _ = Nothing


instance OpenApi.ToSchema Book where
  declareNamedSchema _ = do
    pure $ OpenApi.NamedSchema (Just "Book") $ mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.enum_ ?~ values
    where
      values =
        [minBound..maxBound]
          & fmap (String . bookToText)


instance ToJSON Book where
  toJSON book = String (bookToText book)


genesisParser :: Monad m => ParsecT Text u m Book
genesisParser = string "Genesis" $> Genesis

exodusParser :: Monad m => ParsecT Text u m Book
exodusParser = string "Exodus" $> Exodus

leviticusParser :: Monad m => ParsecT Text u m Book
leviticusParser = string "Leviticus" $> Leviticus

numbersParser :: Monad m => ParsecT Text u m Book
numbersParser = string "Numbers" $> Numbers

deuteronomyParser :: Monad m => ParsecT Text u m Book
deuteronomyParser = string "Deuteronomy" $> Deuteronomy

joshuaParser :: Monad m => ParsecT Text u m Book
joshuaParser = string "Joshua" $> Joshua

judgesParser :: Monad m => ParsecT Text u m Book
judgesParser = string "Judges" $> Judges

ruthParser :: Monad m => ParsecT Text u m Book
ruthParser = string "Ruth" $> Ruth

fstSamuelParser :: Monad m => ParsecT Text u m Book
fstSamuelParser = string "1 Samuel" $> FstSamuel

sndSamuelParser :: Monad m => ParsecT Text u m Book
sndSamuelParser = string "2 Samuel" $> SndSamuel

fstKingsParser :: Monad m => ParsecT Text u m Book
fstKingsParser = string "1 Kings" $> FstKings

sndKingsParser :: Monad m => ParsecT Text u m Book
sndKingsParser = string "2 Kings" $> SndKings

fstChroniclesParser :: Monad m => ParsecT Text u m Book
fstChroniclesParser = string "1 Chronicles" $> FstChronicles

sndChroniclesParser :: Monad m => ParsecT Text u m Book
sndChroniclesParser = string "2 Chronicles" $> SndChronicles

ezraParser :: Monad m => ParsecT Text u m Book
ezraParser = string "Ezra" $> Ezra

nehemiahParser :: Monad m => ParsecT Text u m Book
nehemiahParser = string "Nehemiah" $> Nehemiah

estherParser :: Monad m => ParsecT Text u m Book
estherParser = string "Esther" $> Esther

jobParser :: Monad m => ParsecT Text u m Book
jobParser = string "Job" $> Job

psalmParser :: Monad m => ParsecT Text u m Book
psalmParser = string "Psalms" $> Psalms

proverbsParser :: Monad m => ParsecT Text u m Book
proverbsParser = string "Proverbs" $> Proverbs

ecclesiastesParser :: Monad m => ParsecT Text u m Book
ecclesiastesParser = string "Ecclesiastes" $> Ecclesiastes

songOfSolomonParser :: Monad m => ParsecT Text u m Book
songOfSolomonParser = string "SongOfSolomon" $> SongOfSolomon

isaiahParser :: Monad m => ParsecT Text u m Book
isaiahParser = string "Isaiah" $> Isaiah

jeremiahParser :: Monad m => ParsecT Text u m Book
jeremiahParser = string "Jeremiah" $> Jeremiah

lamentationsParser :: Monad m => ParsecT Text u m Book
lamentationsParser = string "Lamentations" $> Lamentations

ezekielParser :: Monad m => ParsecT Text u m Book
ezekielParser = string "Ezekiel" $> Ezekiel

danielParser :: Monad m => ParsecT Text u m Book
danielParser = string "Daniel" $> Daniel

hoseaParser :: Monad m => ParsecT Text u m Book
hoseaParser = string "Hosea" $> Hosea

joelParser :: Monad m => ParsecT Text u m Book
joelParser = string "Joel" $> Joel

amosParser :: Monad m => ParsecT Text u m Book
amosParser = string "Amos" $> Amos

obadiahParser :: Monad m => ParsecT Text u m Book
obadiahParser = string "Obadiah" $> Obadiah

jonahParser :: Monad m => ParsecT Text u m Book
jonahParser = string "Jonah" $> Jonah

micahParser :: Monad m => ParsecT Text u m Book
micahParser = string "Micah" $> Micah

nahumParser :: Monad m => ParsecT Text u m Book
nahumParser = string "Nahum" $> Nahum

habakkukParser :: Monad m => ParsecT Text u m Book
habakkukParser = string "Habakkuk" $> Habakkuk

zephaniahParser :: Monad m => ParsecT Text u m Book
zephaniahParser = string "Zephaniah" $> Zephaniah

haggaiParser :: Monad m => ParsecT Text u m Book
haggaiParser = string "Haggai" $> Haggai

zechariahParser :: Monad m => ParsecT Text u m Book
zechariahParser = string "Zechariah" $> Zechariah

malachiParser :: Monad m => ParsecT Text u m Book
malachiParser = string "Malachi" $> Malachi

matthewParser :: Monad m => ParsecT Text u m Book
matthewParser = string "Matthew" $> Matthew

markParser :: Monad m => ParsecT Text u m Book
markParser = string "Mark" $> Mark

lukeParser :: Monad m => ParsecT Text u m Book
lukeParser = string "Luke" $> Luke

johnParser :: Monad m => ParsecT Text u m Book
johnParser = string "John" $> John

actsParser :: Monad m => ParsecT Text u m Book
actsParser = string "Acts" $> Acts

romansParser :: Monad m => ParsecT Text u m Book
romansParser = string "Romans" $> Romans

fstCorinthiansParser :: Monad m => ParsecT Text u m Book
fstCorinthiansParser = string "1 Corinthians" $> FstCorinthians

sndCorinthiansParser :: Monad m => ParsecT Text u m Book
sndCorinthiansParser = string "2 Corinthians" $> SndCorinthians

galatiansParser :: Monad m => ParsecT Text u m Book
galatiansParser = string "Galatians" $> Galatians

ephesiansParser :: Monad m => ParsecT Text u m Book
ephesiansParser = string "Ephesians" $> Ephesians

philippiansParser :: Monad m => ParsecT Text u m Book
philippiansParser = string "Philippians" $> Philippians

colossiansParser :: Monad m => ParsecT Text u m Book
colossiansParser = string "Colossians" $> Colossians

fstThessaloniansParser :: Monad m => ParsecT Text u m Book
fstThessaloniansParser = string "1 Thessalonians" $> FstThessalonians

sndThessaloniansParser :: Monad m => ParsecT Text u m Book
sndThessaloniansParser = string "2 Thessalonians" $> SndThessalonians

fstTimothyParser :: Monad m => ParsecT Text u m Book
fstTimothyParser = string "1 Timothy" $> FstTimothy

sndTimothyParser :: Monad m => ParsecT Text u m Book
sndTimothyParser = string "2 Timothy" $> SndTimothy

titusParser :: Monad m => ParsecT Text u m Book
titusParser = string "Titus" $> Titus

philemonParser :: Monad m => ParsecT Text u m Book
philemonParser = string "Philemon" $> Philemon

hebrewsParser :: Monad m => ParsecT Text u m Book
hebrewsParser = string "Hebrews" $> Hebrews

jamesParser :: Monad m => ParsecT Text u m Book
jamesParser = string "James" $> James

fstPeterParser :: Monad m => ParsecT Text u m Book
fstPeterParser = string "1 Peter" $> FstPeter

sndPeterParser :: Monad m => ParsecT Text u m Book
sndPeterParser = string "2 Peter" $> SndPeter

fstJohnParser :: Monad m => ParsecT Text u m Book
fstJohnParser = string "1 John" $> FstJohn

sndJohnParser :: Monad m => ParsecT Text u m Book
sndJohnParser = string "2 John" $> SndJohn

trdJohnParser :: Monad m => ParsecT Text u m Book
trdJohnParser = string "3 John" $> TrdJohn

judeParser :: Monad m => ParsecT Text u m Book
judeParser = string "Jude" $> Jude

revelationParser :: Monad m => ParsecT Text u m Book
revelationParser = string "Revelation" $> Revelation

bookParser :: Monad m => ParsecT Text u m Book
bookParser =
  genesisParser
  <|> try exodusParser
  <|> try leviticusParser
  <|> try numbersParser
  <|> try deuteronomyParser
  <|> try joshuaParser
  <|> try judgesParser
  <|> try ruthParser
  <|> try fstSamuelParser
  <|> try sndSamuelParser
  <|> try fstKingsParser
  <|> try sndKingsParser
  <|> try fstChroniclesParser
  <|> try sndChroniclesParser
  <|> try ezraParser
  <|> try nehemiahParser
  <|> try estherParser
  <|> try jobParser
  <|> try psalmParser
  <|> try proverbsParser
  <|> try ecclesiastesParser
  <|> try songOfSolomonParser
  <|> try isaiahParser
  <|> try jeremiahParser
  <|> try lamentationsParser
  <|> try ezekielParser
  <|> try danielParser
  <|> try hoseaParser
  <|> try joelParser
  <|> try amosParser
  <|> try obadiahParser
  <|> try jonahParser
  <|> try micahParser
  <|> try nahumParser
  <|> try habakkukParser
  <|> try zephaniahParser
  <|> try haggaiParser
  <|> try zechariahParser
  <|> try malachiParser
  <|> try matthewParser
  <|> try markParser
  <|> try lukeParser
  <|> try johnParser
  <|> try actsParser
  <|> try romansParser
  <|> try fstCorinthiansParser
  <|> try sndCorinthiansParser
  <|> try galatiansParser
  <|> try ephesiansParser
  <|> try philippiansParser
  <|> try colossiansParser
  <|> try fstThessaloniansParser
  <|> try sndThessaloniansParser
  <|> try fstTimothyParser
  <|> try sndTimothyParser
  <|> try titusParser
  <|> try philemonParser
  <|> try hebrewsParser
  <|> try jamesParser
  <|> try fstPeterParser
  <|> try sndPeterParser
  <|> try fstJohnParser
  <|> try sndJohnParser
  <|> try trdJohnParser
  <|> try judeParser
  <|> try revelationParser


data VerseRefParser = MkVerseRefParser
  { chapterStart :: Int
  , chapterEnd :: Int
  , verseStart :: Int
  , verseEnd :: Maybe Int
  }
  deriving (Show)

number :: Monad m => ParsecT Text u m Int
number = do
  digits <- many1 digit
  let n = foldl' (\x d -> 10*x + digitToInt d) 0 digits
  seq n (pure n)


data ColonOrDash = Colon | Dash | End

colonOrDashParser :: Monad m => ParsecT Text u m ColonOrDash
colonOrDashParser =
  (char ':' $> Colon)
  <|> (char '-' $> Dash)
  <|> (char '\8211' $> Dash)
  <|> (eof $> End)


verseRefParser :: Monad m => ParsecT Text u m VerseRefParser
verseRefParser = do
  startChapter <- number
  cod <- colonOrDashParser
  case cod of
    Colon -> do
      -- something like "23:2.."
      startVerse <- number
      isMore <- optionMaybe (try (char '-'))
      case isMore of
        Nothing -> do
          -- something like "23:2"
          pure $ MkVerseRefParser startChapter startChapter startVerse Nothing
        Just _ -> do
          -- something like "23:2-.."
          num <- number
          isChapter <- optionMaybe (try (char ':'))
          case isChapter of
            Nothing -> do
              -- something like "23:2-3"
              pure $ MkVerseRefParser startChapter startChapter startVerse (Just num)
            Just _ -> do
              -- something like "23:2-24:4"
              num2 <- number
              pure $ MkVerseRefParser startChapter num startVerse (Just num2)
    Dash -> do
      -- something like "23-.."
      endChapter <- number
      isMore <- optionMaybe (try (char ':'))
      case isMore of
        Nothing ->
          -- something like "23-24"
          pure $ MkVerseRefParser startChapter endChapter 1 Nothing
        Just _ -> do
          -- something like "23-24:4"
          endVerse <- number
          pure $ MkVerseRefParser startChapter endChapter 1 (Just endVerse)
    End ->
      pure $ MkVerseRefParser startChapter startChapter 1 Nothing


data BibleRef = MkBibleRef
  { book :: Book
  , verses :: VerseRefParser
  }
  deriving (Show)


bibleRefParser :: Monad m => ParsecT Text u m BibleRef
bibleRefParser = do
  book <- bookParser
  void $ char ' '
  verses <- verseRefParser
  pure $ MkBibleRef book verses


data Verse = Verse
  { book :: Book
  , chapter :: Int
  , verse :: Int
  , passage :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data PState = MkPState
  { book :: Book
  , prevChapter :: Int
  , prevVerse :: Int
  }

passageParser :: Monad m => ParsecT Text PState m Verse
passageParser = do
  MkPState{book, prevChapter, prevVerse} <- getState
  void $ char '['
  verse <- number
  void $ char ']'
  passage <- many1 (satisfy (/= '['))
  let chapState =
        if verse < prevVerse
         then prevChapter + 1
         else prevChapter
  putState $ MkPState book chapState verse
  pure $ Verse book chapState verse (T.strip (toText passage))


passagesParser :: Monad m => ParsecT Text PState m [Verse]
passagesParser =
  many1 passageParser
