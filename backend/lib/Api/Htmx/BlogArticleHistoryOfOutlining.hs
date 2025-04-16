module Api.Htmx.BlogArticleHistoryOfOutlining where

import Prelude hiding ((**))
import Api.Htmx.Ginger (basicTemplate)
import Web.Scotty.Trans hiding (scottyT)
import Api.Htmx.AuthHelper (getUser)
import DbHelper (MonadDb)



getBlogArticleHistoryOfOutlining
  :: (MonadDb env m, MonadLogger m)
  => ActionT m ()
getBlogArticleHistoryOfOutlining = do
  user <- getUser
  logInfoSH user
  basicTemplate
    "blogArticleHistoryOfOutlining.html"
    identity


