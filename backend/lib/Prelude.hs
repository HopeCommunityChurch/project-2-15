{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module Relude.Base
  , module Relude.Debug
  , module Relude.Bool
  , module Relude.String
  , module Relude.Lifted.Terminal
  , module Relude.Monoid
  , module Relude.Monad.Maybe
  , module Relude.Monad.Either
  , module Relude.Monad.Trans
  , module Relude.Monad.Reexport
  , module Relude.Foldable
  , module Relude.Function
  , module Relude.Functor
  , module Relude.Functor.Fmap
  , module Relude.Functor.Reexport
  , module Relude.Applicative
  , module Relude.Numeric
  , module Relude.Container.Reexport
  , module Control.Lens
  , module UnliftIO
  , module Control.Monad.Logger.CallStack
  , module Data.Time
  , module Data.Time.Zones
  , module Data.Aeson
  , module Deriving.Aeson.Stock
  , module Data.OpenApi
  , module Data.Vector
  , List
  , getCurrentTime
  , unwrap
  )where


import Control.Lens (
  Iso,
  Iso',
  Lens,
  Lens',
  Wrapped (..),
  view,
  (+~),
  (-~),
  (.~),
  (?~),
  (^.),
 )
import Control.Monad.Logger.CallStack (
  LoggingT,
  MonadLogger,
  logDebug,
  logDebugSH,
  logError,
  logErrorSH,
  logInfo,
  logInfoSH,
  logWarn,
  logWarnSH,
  runStdoutLoggingT,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (Day (..), LocalTime (..), UTCTime (..))
import Data.Time qualified as Time
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import Data.Vector (Vector)
import Deriving.Aeson.Stock (Prefixed)
import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container.Reexport
import Relude.Debug
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Functor.Fmap
import Relude.Functor.Reexport
import Relude.Lifted.Terminal
import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
import Relude.Monad.Trans
import Relude.Monoid
import Relude.Numeric
import Relude.String
import UnliftIO hiding (timeout, Handler)

-- import Data.Generics.Labels


type List a = [a]

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

unwrap :: (Wrapped a) => a -> Unwrapped a
unwrap = view _Wrapped'
