{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Voobly.DB where


import RIO

import Data.Acid
import Data.SafeCopy
import Network.HTTP.Client
import Control.Monad.State
import qualified Control.Lens as L


query' :: MonadIO m => QueryEvent event => AcidState (EventState event) -> event -> m (EventResult event)
query' a b = liftIO $ query a b

update' :: MonadIO m => UpdateEvent event => AcidState (EventState event) -> event -> m (EventResult event)
update' a b = liftIO $ update a b

data DB = DB {
  _dbCookies :: [Cookie]
}
L.makeLenses ''DB


emptyDb :: DB
emptyDb = DB {
  _dbCookies = []
}

updateDB :: DB -> Update DB ()
updateDB db = put db

getDB :: Query DB DB
getDB = ask


updateCookies :: [Cookie] -> Update DB ()
updateCookies cookies = modify (L.set dbCookies cookies)

getCookies :: Query DB [Cookie]
getCookies = L.view dbCookies <$> ask


$(deriveSafeCopy 0 'base ''Cookie)
$(deriveSafeCopy 0 'base ''DB)

$(makeAcidic ''DB [
  'updateDB,
  'getDB,
  'updateCookies,
  'getCookies
  ])


