{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prelude
  ( module Relude.Base
  , module Relude.Enum
  , module Relude.Debug
  , module Relude.Bool
  , module Relude.String
  , module Relude.Lifted.Terminal
  , module Relude.Monoid
  , module Relude.List.Reexport
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
  , filter
  )where


import Control.Lens (
  Iso,
  Iso',
  Lens,
  Lens',
  Wrapped (..),
  preview,
  view,
  (+~),
  (-~),
  (.~),
  (?~),
  (^.),
  _Left,
  _Right,
 )
import Control.Monad.Logger.CallStack (
  LoggingT,
  MonadLogger (..),
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
import Control.Monad.Logger.Prefix (LogPrefixT)
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
import Relude.Enum
import Relude.Foldable
import Relude.Function hiding (id)
import Relude.Functor
import Relude.Functor.Fmap
import Relude.Functor.Reexport
import Relude.Lifted.Terminal
import Relude.List.Reexport
import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
import Relude.Monad.Trans
import Relude.Monoid
import Relude.Numeric
import Relude.String
import UnliftIO hiding (Handler, timeout)
import Web.Scotty.Trans (ActionT)

-- import Data.Generics.Labels


type List a = [a]

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

unwrap :: (Wrapped a) => a -> Unwrapped a
unwrap = view _Wrapped'

instance MonadIO m => MonadFail (LogPrefixT m) where
  fail :: MonadIO m => String -> LogPrefixT m a
  fail = liftIO . fail

instance (MonadLogger m) => MonadLogger (ActionT m) where
  monadLoggerLog loc src lvl msg =
    lift $ monadLoggerLog loc src lvl msg

