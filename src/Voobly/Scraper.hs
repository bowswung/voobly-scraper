module Voobly.Scraper where


import Voobly.DB

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified RIO.Vector as V
import RIO.Time
import Data.Acid
import Data.Tree
import Data.Acid.Local
import Options.Applicative
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Control.Monad.State.Strict
import qualified Text.HTML.Parser       as P
import qualified Text.HTML.Tree       as P
import qualified Data.Csv as Csv

import qualified RIO.List.Partial as Partial
import qualified RIO.List as L

import qualified Data.IxSet.Typed as IxSet
import qualified  Text.Regex as Regex

data AppError =
    AppErrorCouldntLogIn Text
  | AppErrorInvalidHtml Text
  | AppErrorParserError Text
  deriving (Show, Typeable)

instance Exception AppError

data Command =
    CommandRun
  | CommandQuery

data Options = Options
  { username   :: Text
  , password   :: Text
  , debug :: Bool
  , runCommand :: Command
  }


optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (
       long "username"
    <> short 'u'
    <> help "Voobly username"
    <> value ""
    )
  <*> strOption (
       long "password"
    <> short 'p'
    <> help "Voobly password"
    <> value ""
    )
  <*> switch (
       long "debug"
    <> help "For debugging"
    )
   <*> subparser (
      ( command "run"          (info (helper <*> pure CommandRun)                 (progDesc "Run the scraper" ))
     <> command "query"  (info (helper <*> pure CommandQuery)          (progDesc "Show data"))
      )
    )


optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper) fullDesc

data AppEnv = AppEnv {
  appEnvLogFunction :: LogFunc,
  appEnvOptions :: Options,
  appEnvAcid :: AcidState DB
  }

data AppState = AppState {
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


withAcid :: (AcidState DB -> IO a) -> IO a
withAcid = bracket openState closeState
  where
    openState :: IO (AcidState DB)
    openState = openLocalStateFrom acidDir emptyDb

    closeState :: AcidState DB -> IO ()
    closeState acid = liftIO $ createCheckpointAndClose acid


runScraper :: IO ()
runScraper = do
  options <- execParser optionsParserInfo
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogUseTime True logOptions
  manager <- liftIO $ newManager tlsManagerSettings
  withLogFunc logOptions' $ \lf -> do
    withAcid $ \acid -> do
      let appEnv = AppEnv lf options acid
          appState = AppState manager
      runRIO appEnv $ runAppM appState $ do
        case runCommand options of
          CommandRun -> do
            initialise
            logInfo $ "*** Scraping ladder " <> displayShow LadderRm <> " ***"
            scrapeLadder LadderRm
            scrapePlayers
          CommandQuery -> do
            db <- query' GetDB
            logDebug $ "Players: " <> (displayShow $ (IxSet.size $ _dbPlayers db))
            logDebug $ displayShow $ take 20 (IxSet.toList $ _dbPlayers db)


runDir :: FilePath
runDir = "./run"

acidDir :: FilePath
acidDir = runDir <> "/state"

vooblyUrl :: Text
vooblyUrl = "https://www.voobly.com"

vooblyAuthUrl :: Text
vooblyAuthUrl = vooblyUrl <> "/login/auth"

debugResponse :: Text -> Request -> Response BL.ByteString -> AppM ()
debugResponse text req res = do
  let outPath = runDir <> "/" <> T.unpack text <> ".html"
  BL.writeFile outPath (responseBody res)
  logDebug $ (displayShow $ method req) <> " request to " <> (displayShow $ path req) <> " completed with status code " <> (displayShow $ responseStatus res)
  logDebug $ "Response body written to " <> displayShow outPath <> ".html"

initialise :: AppM ()
initialise = do
  -- first test if we are already logged in
  l <- isLoggedIn
  case l of
    True -> logInfo "Already logged in"
    False -> do
      logInfo "Not logged in, authenticating now"
      authenticate


isLoggedIn :: AppM Bool
isLoggedIn = do
  req <- parseRequest $ T.unpack $ vooblyUrl <> "/profile"
  res <- makeRequest $ req
  let resText = decodeUtf8With lenientDecode $ BL.toStrict $ responseBody res
  return . not $  T.isInfixOf "You must login to access this page." resText

makeSetupRequest :: AppM ()
makeSetupRequest = do
  req <- parseRequest $ T.unpack $ vooblyUrl
  _res <- makeRequest $ req
  return ()

authenticate :: AppM ()
authenticate = do
  makeSetupRequest
  opts <- fmap appEnvOptions ask
  baseReq <- parseRequest $ T.unpack $ vooblyAuthUrl
  let req = urlEncodedBody [("username", T.encodeUtf8 . username $ opts  ), ("password", T.encodeUtf8 . password $ opts )] baseReq
  _res <- makeRequest req
  l <- isLoggedIn
  if l
    then return ()
    else throwM $ AppErrorCouldntLogIn "Tried with supplied username and password"



ladderPageUrl :: Ladder -> Int -> Text
ladderPageUrl l p = vooblyUrl <> "/ladder/ranking/" <> T.pack (show $ ladderId l) <> "/" <> T.pack (show p)


scrapeLadder :: Ladder -> AppM ()
scrapeLadder l = do
  skip <- do
    appEnv <- ask
    if (debug . appEnvOptions $  appEnv)
      then do
        db <- query' GetDB
        if (IxSet.size $ _dbPlayers db)  > 100
          then pure True
          else pure False
      else pure False
  if skip
    then do
      logDebug $ "Skipping scrape ladder for --debug because we have enough players already"
    else do
      startProgress <- fmap (fromMaybe $ defaultPlayerLadderProgress l) $ query' (GetPlayerLadderProgress l)
      now <- getCurrentTime
      let p = case playerLadderProgressLastPageHandled startProgress of
                Just i -> i + 1
                Nothing -> 0
          needsUpdate =
            case playerLadderProgressLastCompleted startProgress of
              Nothing -> True
              Just d -> diffUTCTime now d  > (3600 * 24 * 7) -- every week
      if needsUpdate
        then do
          tags <- makeHTMLTreesRequest $ ladderPageUrl l p
          table <- extractLadderTable tags
          let rows = drop 1 $ extractFromTree table (isTagOpen "tr")
          tups <- mapM extractLadderRow rows

          if null tups
            then do
              logInfo $ "Page " <> displayShow p <> " didn't contain any data - ladder is finished!"
              update' $ UpdatePlayerLadderProgress (startProgress{playerLadderProgressLastPageHandled = Nothing, playerLadderProgressLastCompleted = Just now})
            else do
              void $ mapM (updatePlayerLadders l) tups
              logInfo $ "Page " <> displayShow p <> " processed successfully with " <> (displayShow $ length tups) <> " players (e.g." <> displayShow (Partial.head tups)
              update' $ UpdatePlayerLadderProgress (startProgress{playerLadderProgressLastPageHandled = Just p})
              scrapeLadder l
        else do
          logInfo $ "Player ladder " <> displayShow l <> " is up to date as of " <> displayShow (playerLadderProgressLastCompleted startProgress) <> " and does not need to be updated now"
          return ()


scrapePlayers :: AppM ()
scrapePlayers = do
  db <- query' GetDB
  now <- getCurrentTime
  let weekago = addUTCTime (-3600 * 24 * 7) now
  let playersToUpdate = IxSet.toList $ IxSet.getLT (Just weekago) $ _dbPlayers db
  void $ mapM scrapePlayer playersToUpdate


playerMatchUrl :: Player -> Int -> Text
playerMatchUrl p page = vooblyUrl <> "/profile/view/" <> (playerIdToText . playerId $ p) <> "/Matches/games/matches/user/" <> (playerIdToText . playerId $ p) <> "/0/" <> T.pack (show page)

divInt :: Int -> Int -> Double
divInt = (/) `on` fromIntegral

scrapePlayer :: Player -> AppM ()
scrapePlayer p = do
  r <- makeTextRequest $ playerMatchUrl p 0
  totalMatches <- extractMatchCount r
  let matchesMissing = totalMatches - (V.length $ playerMatchIds p)
      pagesToRequest = ceiling $ matchesMissing `divInt` 10
  if pagesToRequest < 1
    then do
      logDebug $ "no new matches for player " <> displayShow p
      return ()
    else do
      let pages = reverse [1 .. pagesToRequest]
      void $ mapM (scrapePlayerPage p) pages


scrapePlayerPage :: Player -> Int -> AppM ()
scrapePlayerPage p page = do
  r <- makeTextRequest $ playerMatchUrl p 0
  matchIds <- extractPlayerMatchIds r
  logDebug $ displayShow matchIds

extractMatchCount :: Text -> AppM Int
extractMatchCount t = do
  let r = Regex.mkRegex "<div class=\"count\">Displaying [0-9]+ - [0-9]+ out of ([0-9]+) matches"
  case Regex.matchRegex r (T.unpack t) of
    Just [x] -> runParserFromText $ T.pack x
    _ -> throwM $ AppErrorInvalidHtml "Expected regex to match exactly one numeric value in extractMatchCount"





extractPlayerMatchIds :: Text -> AppM [Int]
extractPlayerMatchIds t = do
  let r = Regex.mkRegex "<td bgcolor=\"#[^\"]+\" style=\"\"><a href=\"https:\/\/voobly\.com\/match\/view\/17649526\">#([0-9]+)<\/a><\/td>"
  case Regex.matchRegex r (T.unpack t) of
    Just xs ->
      if length xs > 0 && length xs < 11
        then runParserFromText $ T.pack x
        else throwM $ AppErrorInvalidHtml "Expected between 1 and 10 match ids"
    _ -> throwM $ AppErrorInvalidHtml "Expected regex to match exactly one numeric value in extractMatchCount"




type LadderRow = (Text, PlayerId, Int, Int, Int)



updatePlayerLadders :: Ladder -> LadderRow -> AppM ()
updatePlayerLadders l (name, pid, rating, wins, loss) = do
  let p = Player pid name V.empty Nothing
  update' $ UpdatePlayer p
  let pl = PlayerLadder{
             playerLadderPlayerId = pid
           , playerLadderLadder = l
           , playerLadderRating = rating
           , playerLadderWins = wins
           , playerLadderLoss = loss
           }
  update' $ UpdatePlayerLadder pl



extractLadderRow :: Tree P.Token -> AppM LadderRow
extractLadderRow (Node _ forest) =
  case extractFromForest forest (isTagOpen "td") of
    _c1:c2:c3:c4:c5:_c6:_c7:[] -> do
      (name, pid) <- extractNameAndIdFromToken c2
      rating <- extractIntFromToken c3
      wins <- extractIntFromToken c4
      loss <- extractIntFromToken c5
      return (name, pid, rating, wins, loss)
    _ -> throwM $ AppErrorInvalidHtml "Expected 7 tokens in ladder row"


extractIntFromToken :: Tree P.Token -> AppM Int
extractIntFromToken tok =
  case flattenForest $ extractFromTree tok (isContextText) of
    [P.ContentText t] -> runParserFromText t
    _ -> throwM $ AppErrorInvalidHtml "Expected one contenttext token for extracting an Int"

extractNameAndIdFromToken :: Tree P.Token -> AppM (Text, PlayerId)
extractNameAndIdFromToken t =
  case flatten $ Partial.last $ extractFromTree t (isTagOpen "a") of
    (P.TagOpen _ attrs):(P.ContentText name):[] -> do

      case findAttributeValue "href" attrs of
        Nothing -> throwM $ AppErrorInvalidHtml "Expected href attribute in extractNameAndIdFromToken"
        Just href -> do
          let tId = Partial.last $ T.split (== '/') href
          return (name, PlayerId . T.strip $ tId)



    _ -> throwM $ AppErrorInvalidHtml "Expected open tag, contenttext for extractNameAndIdFromToken"



runParserFromText :: (Csv.FromField a) => Text -> AppM a
runParserFromText t =
  case Csv.runParser . Csv.parseField $ encodeUtf8 t of
    Left err -> throwM $ AppErrorParserError $ T.pack err
    Right a -> return a

findAttributeValue :: Text -> [P.Attr] -> Maybe Text
findAttributeValue x ls = do
  (P.Attr _ v) <- L.find (\(P.Attr n _) -> n == x) ls
  pure v



isTagOpen :: Text -> P.Token -> Bool
isTagOpen x (P.TagOpen t _) = x == t
isTagOpen _ _ = False

isContextText :: P.Token -> Bool
isContextText (P.ContentText _) = True
isContextText _ = False

flattenForest :: Forest a -> [a]
flattenForest = concat . map flatten

extractFromForest :: Forest a -> (a -> Bool) -> Forest a
extractFromForest f x = concat $ map (\t -> extractFromTree t x) f

extractFromTree :: Tree a -> (a -> Bool) -> Forest a
extractFromTree tree@(Node a forest) f =
  let l = if f a then [tree] else []
  in l ++ (concat $ map (\t -> extractFromTree t f) forest)

makeHTMLTreesRequest :: Text -> AppM ([P.Token])
makeHTMLTreesRequest u = do
  baseReq <- parseRequest $ T.unpack $ u
  res <- makeRequest baseReq
  return $ P.canonicalizeTokens $ P.parseTokens $ decodeUtf8With lenientDecode (BL.toStrict $ responseBody res)

makeTextRequest :: Text -> AppM Text
makeTextRequest u = do
  baseReq <- parseRequest $ T.unpack $ u
  res <- makeRequest baseReq
  return $ decodeUtf8With lenientDecode (BL.toStrict $ responseBody res)

extractLadderTable :: [P.Token] -> AppM (Tree P.Token)
extractLadderTable ts = do
  let (_,_,_,res) =
        (flip execState) (False, False, False, []) $ mapM collectToken ts
  case P.tokensToForest res of
    Left e -> throwM $ AppErrorInvalidHtml $ utf8BuilderToText . displayShow $ e
    Right [f] -> return f
    Right _ -> throwM $ AppErrorInvalidHtml "Expected exactly one table tree"

  where
    collectToken :: P.Token -> State (Bool, Bool, Bool, [P.Token]) ()
    collectToken tok = do
      (afterLadder, inTable, lastTagWasImg, res) <- get
      case tok of
        P.ContentText t -> do
          if (afterLadder && inTable)
            then put (afterLadder, inTable, False, res ++ [tok])
            else if t == "Ladder Players"
              then put (True, inTable, False, res)
              else return ()
        P.TagOpen t _ -> do
          if (afterLadder && inTable)
            then put (afterLadder, inTable, t == "img", res ++ [tok])
            else if (afterLadder && t == "table")
              then put (afterLadder, True, False, res ++ [tok])
              else return ()
        P.TagClose t  -> do
          if inTable && lastTagWasImg && t == "a"
            then put (afterLadder, inTable, False, res)
            else
              if (afterLadder && t == "table")
                then put (False, False, False, res ++ [tok])
                else if afterLadder && inTable
                  then put (afterLadder, inTable, False, res ++ [tok])
                  else return ()
        _ -> if afterLadder && inTable
              then put (afterLadder, inTable, False, res ++ [tok])
              else return ()




defaultRequestHeaders :: [Header]
defaultRequestHeaders = [
    ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36"),
    ("Origin", "https://www.voobly.com")
  ]

makeRequest :: Request -> AppM (Response BL.ByteString)
makeRequest req = do
  st <- get
  origCookieJar <- fmap createCookieJar $ query' GetCookies
  let req' = req {
        cookieJar = Just origCookieJar,
        requestHeaders = requestHeaders req ++ defaultRequestHeaders
      }
  res <- liftIO $ httpLbs req' (appStateManager st)
  update' $ UpdateCookies (destroyCookieJar $ responseCookieJar res)
  return res



query' :: (QueryEvent event, EventState event ~ DB) => event -> AppM (EventResult event)
query' e = do
  env <- ask
  liftIO $ query (appEnvAcid env) e

update' :: (UpdateEvent event, EventState event ~ DB) => event -> AppM (EventResult event)
update' e = do
  env <- ask
  liftIO $ update (appEnvAcid env) e


