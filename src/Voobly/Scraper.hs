module Voobly.Scraper where


import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import  RIO.Time

import Options.Applicative
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Control.Monad.State.Strict

data Options = Options
  { username   :: Text
  , password   :: Text
  }


optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
       long "username"
    <> short 'u'
    <> help "Voobly username"
    )
 <*> strOption (
       long "password"
    <> short 'p'
    <> help "Voobly password"
    )

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper) fullDesc

data AppEnv = AppEnv {
  appEnvLogFunction :: LogFunc,
  appEnvOptions :: Options
  }

data AppState = AppState {
  appStateCookieJar :: CookieJar,
  appStateManager :: Manager
}
newtype AppM a = AppM { extractAppM :: StateT AppState (RIO AppEnv) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader AppEnv, MonadState AppState)

runAppM :: AppState -> AppM a -> RIO AppEnv a
runAppM st act = do
  (a, _) <- runStateT (extractAppM act) st
  return a

instance HasLogFunc AppEnv where
  logFuncL = lens appEnvLogFunction (\x y -> x { appEnvLogFunction = y })

runScraper :: IO ()
runScraper = do
  options <- execParser optionsParserInfo
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogUseTime True logOptions
  manager <- liftIO $ newManager tlsManagerSettings
  withLogFunc logOptions' $ \lf -> do
    let appEnv = AppEnv lf options
        appState = AppState (createCookieJar []) manager
    runRIO appEnv $ runAppM appState $ do
      authenticate

vooblyUrl :: Text
vooblyUrl = "https://www.voobly.com"

vooblyAuthUrl :: Text
vooblyAuthUrl = vooblyUrl <> "/login/auth"

authenticate :: AppM ()
authenticate = do
  opts <- fmap appEnvOptions ask
  req <- parseRequest $ T.unpack $ vooblyAuthUrl
  let req' = req {
          method = "POST"
       }
      req'' = urlEncodedBody [("username", T.encodeUtf8 . username $ opts  ), ("password", T.encodeUtf8 . password $ opts )] req'
  res <- makeRequest req''
  logDebug $ displayShow res

makeRequest :: Request -> AppM (Response BL.ByteString)
makeRequest req = do
  st <- get
  let req' = req {
        cookieJar = Just $ appStateCookieJar st
      }
  res <- liftIO $ httpLbs req (appStateManager st)
  now <- getCurrentTime
  let (cj, res') = updateCookieJar res req' now (appStateCookieJar st)
  put (st{appStateCookieJar = cj})
  return res'

