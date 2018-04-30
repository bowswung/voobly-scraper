module Voobly.Scraper where


import Voobly.DB

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import qualified RIO.Vector.Boxed as VB

import qualified System.Process as SP

import RIO.Time
import qualified RIO.HashMap as HM

import Data.Acid
import Data.Tree
--import Data.Acid.Local
import Options.Applicative
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

import Control.Monad.State.Strict
import qualified Text.HTML.Parser       as P
import qualified Text.HTML.Tree       as P
import qualified Data.Csv as Csv

import qualified RIO.List as L

import qualified Data.IxSet.Typed as IxSet
import Text.Regex.Posix ((=~))
import qualified Safe as Safe
import qualified Data.Proxy as Proxy
--import Control.Concurrent.Async.Extra
--suimport System.Cron
import qualified RIO.Set as Set
import qualified Control.Monad.Catch as MC
import qualified Control.Concurrent.QSem as S
import System.IO(putStrLn)


data Command =
    CommandRun
  | CommandQuery
  | CommandDump
  | CommandDeleteMatches
  | CommandRunErrors

data Options = Options
  { username   :: Text
  , password   :: Text
  , threadCount :: Int
  , debug :: Bool
  , skipUpdateMatchIds :: Bool
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
  <*> option auto (
       long "threads"
    <> short 't'
    <> help "Number of threads to use"
    <> value 1
    )
  <*> switch (
       long "debug"
    <> help "For debugging"
    )
  <*> switch (
       long "skip-update-match-ids"
    <> help "For skipping match id update"
    )
   <*> subparser (
      ( command "run"          (info (helper <*> pure CommandRun)                 (progDesc "Run the scraper" ))
     <> command "query"  (info (helper <*> pure CommandQuery)          (progDesc "Show data"))
     <> command "dump"  (info (helper <*> pure CommandDump)          (progDesc "Dump data"))
     <> command "deleteMatches"  (info (helper <*> pure CommandDeleteMatches)          (progDesc "Delete all matches from the db"))
     <> command "runErrors"  (info (helper <*> pure CommandRunErrors)          (progDesc "Sequentially run matches that were previously errors."))
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

deriving instance MC.MonadCatch (RIO AppEnv)

newtype AppM a = AppM { extractAppM :: StateT AppState (RIO AppEnv) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MC.MonadCatch, MonadReader AppEnv, MonadState AppState)

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
    closeState acid = do
      putStrLn "***** CLOSING STATE *****"
      liftIO $ createCheckpoint acid
      liftIO $ createArchive acid
      liftIO $ closeAcidState acid
      putStrLn "***** STATE CHECKPOINTED, ARCHIVED and CLOSED *****"



runStack :: AppEnv -> AppState -> AppM a -> IO a
runStack appEnv appState f = runRIO appEnv $ runAppM appState f

stackToIO :: AppEnv -> AppState -> AppM a -> IO a
stackToIO appEnv appState a = runStack appEnv appState a

stackToIO' :: AppEnv -> AppState -> (a -> AppM b) -> a -> IO b
stackToIO' appEnv appState f a = runStack appEnv appState (f a)

doCreateCheckpoint :: AppM ()
doCreateCheckpoint = do
  appEnv <- ask
  logInfo $ "*** START ACID CHECKPOINT ***"
  liftIO $ createCheckpoint $ appEnvAcid appEnv
  liftIO $ createArchive  $ appEnvAcid appEnv
  logInfo $ "*** END ACID CHECKPOINT ***"

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
      runStack appEnv appState $ do
        case runCommand options of
          CommandRun -> do
            -- checkpointTask <- stackToIO doCreateCheckpoint
            -- _ <- liftIO $ execSchedule $ do
            -- addJob checkpointTask "5,20,35,50 * * * *"

            initialise
            scrapeLadder LadderRm
            scrapeLadder LadderRmTeam
            scrapePlayers
            scrapeMatches


          CommandQuery -> do
            db <- query' GetDB
            logDebug $ "Players: " <> (displayShow $ (IxSet.size $ _dbPlayers db))
            logDebug $ displayShow $ take 20 (IxSet.toList $ _dbPlayers db)

          CommandDump -> do
            dumpMatches

          CommandDeleteMatches -> do
            update' DeleteMatches
            logDebug $ "Match database cleared"

          CommandRunErrors -> do
            matchIds <- query' GetMatchIds
            let matchIdsToUpdate = L.sort . HM.keys $ HM.filter isMatchFetchStatusExceptionError matchIds
            logInfo $ "*** " <> (displayShow $ length matchIdsToUpdate) <> " error matches to be scraped ***"
            void $ mapM scrapeMatch matchIdsToUpdate


dumpMatches :: AppM ()
dumpMatches = do
  db <- query' GetDB
  let civMap = HM.fromList $ (map (\c -> (civilisationId c, Csv.toField . civilisationName $ c))) (IxSet.toList ._dbCivilisations $ db)
  rendered <- fmap concat $ mapM (renderMatch civMap) $ IxSet.toAscList (Proxy.Proxy :: Proxy.Proxy MatchId) (_dbMatches db)
  logInfo $ "Dumping " <> (displayShow . IxSet.size $ _dbMatches db) <> " matches in "  <> (displayShow . length $ rendered) <> " rows to csv"
  let enc = Csv.encodeByNameWith Csv.defaultEncodeOptions headerDef rendered
  let fname = runDir <> "/matchDump.csv"
  BL.writeFile fname enc
  logDebug $ "Matches dumped successfully to " <> (displayShow fname)
  where
    headerDef :: Csv.Header
    headerDef = VB.fromList [
      "MatchId",
      "MatchUrl",
      "MatchDate",
      "MatchDuration",
      "MatchLadder",
      "MatchMap",
      "MatchMods",
      "MatchPlayerId",
      "MatchPlayerName",
      "MatchPlayerTeam",
      "MatchPlayerCivId",
      "MatchPlayerCivName",
      "MatchPlayerWinner",
      "MatchPlayerPreRating",
      "MatchPlayerPostRating"
      ]

    renderMatch ::HM.HashMap CivilisationId ByteString -> Match -> AppM [Csv.NamedRecord]
    renderMatch civMap Match{..} = do
      let matchDetails = [
              "MatchId" Csv..= matchId,
              "MatchUrl" Csv..= matchPageUrl matchId,
              "MatchDate" Csv..= matchDate,
              "MatchDuration" Csv..= matchDuration,
              "MatchLadder" Csv..= matchLadder,
              "MatchMap" Csv..= matchMap,
              "MatchMods" Csv..= matchMods
            ]

      (flip mapM) matchPlayers $ \m ->
        case m of
          MatchPlayer{..} -> do
            p <- query' $ GetPlayer matchPlayerPlayerId
            let pName = fromMaybe "*NameNotFoundInScraperDB*" (fmap playerName p)
                civName = fromMaybe "*CivNameNotFoundInScraperDB*" $ HM.lookup matchPlayerCiv civMap
            return $ Csv.namedRecord $ matchDetails ++ [
                "MatchPlayerId" Csv..= matchPlayerPlayerId,
                "MatchPlayerName" Csv..= pName,
                "MatchPlayerTeam" Csv..= matchPlayerTeam,
                "MatchPlayerCivId" Csv..= matchPlayerCiv,
                "MatchPlayerCivName" Csv..= civName,
                "MatchPlayerWinner" Csv..= matchPlayerWon,
                "MatchPlayerPreRating" Csv..= matchPlayerPreRating,
                "MatchPlayerPostRating" Csv..= matchPlayerPostRating
              ]

          MatchPlayerError t -> do
            let (errorName :: Text) = "*VooblyErrorPlayerNotFound*"
            return $ Csv.namedRecord $ matchDetails ++ [
                "MatchPlayerId" Csv..= errorName,
                "MatchPlayerName" Csv..= t,
                "MatchPlayerTeam" Csv..= errorName,
                "MatchPlayerCivId" Csv..= errorName,
                "MatchPlayerCivName" Csv..= errorName,
                "MatchPlayerWinner" Csv..= errorName,
                "MatchPlayerPreRating" Csv..= errorName,
                "MatchPlayerPostRating" Csv..= errorName
              ]



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

ignore :: a -> b -> Maybe c
ignore _ _ = Nothing

isLoggedIn :: AppM Bool
isLoggedIn = do
  req <- parseRequest $ T.unpack $ vooblyUrl <> "/profile"
  res <- makeRequest $ req
  let resText = decodeUtf8With ignore $ BL.toStrict $ responseBody res
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
  logInfo $ "*** Scraping ladder " <> displayShow l <> " ***"
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
              logInfo $ "Page " <> displayShow p <> " processed successfully with " <> (displayShow $ length tups) <> " players (e.g." <> displayShow (Safe.headMay tups)
              update' $ UpdatePlayerLadderProgress (startProgress{playerLadderProgressLastPageHandled = Just p})
              scrapeLadder l
        else do
          logInfo $ "Player ladder " <> displayShow l <> " is up to date as of " <> displayShow (playerLadderProgressLastCompleted startProgress) <> " and does not need to be updated now"
          return ()


scrapePlayers :: AppM ()
scrapePlayers = do
  logInfo $ "*** Scraping player match ids ***"
  doUpdateMatchIds
  skip <- do
    appEnv <- ask
    if (debug . appEnvOptions $  appEnv)
      then do
        matchIds <- query' GetMatchIds
        if HM.size matchIds > 1000
          then do
            logDebug $ "Skipping scrapePlayers for --debug because we have " <> displayShow (HM.size matchIds) <> " match ids already"
            return True
          else return False
      else return False
  if skip
    then return ()
    else do
      db <- query' GetDB
      now <- getCurrentTime
      let weekago = addUTCTime (-3600 * 24 * 7) now
      let playersToUpdateBase = VB.fromList $ IxSet.toAscList (Proxy.Proxy :: Proxy.Proxy PlayerId) $ IxSet.getLT (Just weekago) $ _dbPlayers db
          totalPlayers = IxSet.size $ _dbPlayers db
      appEnv <- ask
      playersToUpdate <- if (debug . appEnvOptions $  appEnv)
        then do
          logDebug $ "Restricting player scraping to first 20 players for --debug"
          return $ VB.take 20 playersToUpdateBase
        else return playersToUpdateBase
      logInfo $ "*** " <> (displayShow totalPlayers) <> " players in the DB and " <> (displayShow $ VB.length playersToUpdate) <> " players need to be scraped ***"

      appState <- get

      liftIO $ withThreads appEnv (stackToIO' appEnv appState scrapePlayer) playersToUpdate

withThreads :: AppEnv -> (a -> IO ()) -> Vector a -> IO ()
withThreads appEnv ioAct !t = do
  let ioActWrapped = catchAppError . ioAct

  putStrLn $ T.unpack . utf8BuilderToText $ "*** Launching " <> (displayShow . VB.length $ t) <> " actions with " <> displayShow (threadCount . appEnvOptions $ appEnv) <> " threads ***"


  let runner =
          if (debug . appEnvOptions $ appEnv)
            then \x -> VB.mapM_ (ioActWrapped) x
            else \x ->  mapConcurrentlyBounded_ (threadCount . appEnvOptions $ appEnv) ioActWrapped x
      runnerWithMore = \x -> do
        runner x
        putStrLn "***** CHECKPOINTING ACID AFTER 1000 tasks *****"
        createCheckpoint (appEnvAcid appEnv)
        createArchive (appEnvAcid appEnv)
        putStrLn "***** CHECKPOINTING ACID COMPLETED *****"

  liftIO $ mapM_ runnerWithMore $  vChunksOf 1000 t
  where
    catchAppError :: IO c -> IO ()
    catchAppError i = catch (void i) handleAnyException
    handleAnyException ::SomeException -> IO ()
    handleAnyException e = do
      runRIO appEnv $
        logError $ "Error in task: " <> displayShow e

mapConcurrentlyBounded_ :: Traversable t => Int -> (a -> IO b) -> t a -> IO ()
mapConcurrentlyBounded_ bound act items =
    do qs <- S.newQSem bound
       let wrappedAction x =
               bracket_ (S.waitQSem qs) (S.signalQSem qs) (act x)
       mapConcurrently_ wrappedAction items



vChunksOf :: Int -> Vector a -> [(Vector a)]
vChunksOf l v =
  if VB.null v
    then []
    else
      let (batch, next) = VB.splitAt l v
      in batch : vChunksOf l next


{-withThreadsBatch :: (a -> AppM b) -> Vector a -> AppM ()
withThreadsBatch act t = do
  if (debug . appEnvOptions $ appEnv)
    then mapM_ (liftIO . ioActWrapped) $ VB.toList t
    else liftIO $ mapConcurrently_ ioActWrapped t
  where-}



playerMatchUrl :: Player -> Int -> Text
playerMatchUrl p page = vooblyUrl <> "/profile/view/" <> (playerIdToText . playerId $ p) <> "/Matches/games/matches/user/" <> (playerIdToText . playerId $ p) <> "/0/" <> T.pack (show page)

divInt :: Int -> Int -> Double
divInt = (/) `on` fromIntegral

scrapePlayer :: Player -> AppM ()
scrapePlayer p = do
  r <- makeTextRequest $ playerMatchUrl p 0
  logDebug $ "Looking for player games " <> (displayShow $ playerId p) <> " at " <> (displayShow $ playerMatchUrl p 0)

  totalMatches <- extractMatchCount r
  let matchesMissing = totalMatches - (Set.size $ playerMatchIds p)
      pagesToRequest = ceiling $ matchesMissing `divInt` 10
  if pagesToRequest < 1
    then do
      now <- getCurrentTime
      when (totalMatches < 1) (update' $ UpdatePlayer p{playerLastCompletedUpdate = Just now})

      logDebug $ "no new matches for player " <> (displayShow $ playerId p)
      return ()
    else do
      let pages = reverse [1 .. pagesToRequest]
      void $ mapM (scrapePlayerPage p pagesToRequest) pages

doUpdateMatchIds :: AppM ()
doUpdateMatchIds = do
  logInfo $ "*** Updating match ids ***"

  appEnv <- ask
  if (debug . appEnvOptions $  appEnv) || (skipUpdateMatchIds . appEnvOptions $ appEnv)
    then logWarn $ "*** SKIPPING MATCH ID UPDATE FOR DEBUG OR --skip-update-match-ids ***"
    else update' $ UpdateMatchIds
  logInfo $ "*** Done updating match ids ***"







scrapePlayerPage :: Player -> Int -> Int -> AppM ()
scrapePlayerPage p highestPage page  = do
  logDebug $ "Scraping player " <> (displayShow $ playerId p) <> ": page " <> displayShow page
  r <- makeTextRequest $ playerMatchUrl p (page - 1)
  matchIds <- fmap (map MatchId) $ extractPlayerMatchIds (page < highestPage) r
  res <- update' $ AddNewMatchIds (playerId p) matchIds
  case res of
    Just err -> throwM $ AppErrorDBError err
    Nothing -> do
      when (page == 1) $ do
        mFreshP <- query' $ GetPlayer (playerId p)
        case mFreshP of
          Nothing -> throwM $ AppErrorNotFound $ "Could not find player for id" <> (utf8BuilderToText . displayShow $ playerId p)
          Just freshP -> do
            now <- getCurrentTime
            update' $ UpdatePlayer freshP{playerLastCompletedUpdate = Just now}








extractMatchCount :: Text -> AppM Int
extractMatchCount t = do
  case doRegexJustCaptureGroups (T.unpack t)  "<div class=\"count\">Displaying [0-9]+ - [0-9]+ out of ([0-9]+) matches" of
    [x] -> runParserFromText $ T.pack x
    [] ->
      if not . null $ doRegex (T.unpack t) "<div class=\"count\">No matches found</div>"
        then return 0
        else
          case doRegexJustCaptureGroups (T.unpack t)  "<div class=\"count\">Found ([a-zA-Z]+) match[a-z]*</div>" of
            [strNum] -> strNumToInt strNum

            _ -> throwM $ AppErrorInvalidHtml $ "Expected to find either numeric or number string or no matches found in extractMatchCount"
    x -> throwM $ AppErrorInvalidHtml $ "Expected regex to match exactly one numeric value in extractMatchCount, got " <> (utf8BuilderToText . displayShow $ x)

strNumToInt :: String -> AppM Int
strNumToInt "one" = pure 1
strNumToInt "two" = pure 1
strNumToInt "three" = pure 1
strNumToInt "four" = pure 1
strNumToInt "five" = pure 1
strNumToInt "six" = pure 1
strNumToInt "seven" = pure 1
strNumToInt "eight" = pure 1
strNumToInt "nine" = pure 1
strNumToInt "ten" = pure 1
strNumToInt a = throwM  $ AppErrorInvalidHtml $ "Could not convert " <> (utf8BuilderToText . displayShow $ a) <> " to int in strNumToInt from extractMatchCount"



doRegex :: String -> String -> [[String]]
doRegex a b = a =~ b

doRegexJustCaptureGroups :: String -> String -> [String]
doRegexJustCaptureGroups a b = concat $ map (drop 1) (doRegex a b)


extractPlayerMatchIds :: Bool -> Text -> AppM [Int]
extractPlayerMatchIds expectTen t = do
  case doRegexJustCaptureGroups (T.unpack t) "<td bgcolor=\"[^\"]*\" style=\"[^\"]*\"><a href=\"https://voobly.com/match/view/[0-9]+\">#([0-9]+)</a></td>" of
    xs -> do
      if (not expectTen && (length xs < 1 || length xs > 11)) || (expectTen && not (length xs == 10))
        then if expectTen
          then throwM $ AppErrorInvalidHtml "Expected between exactly 10 match ids"
          else throwM $ AppErrorInvalidHtml "Expected between exactly 1 and 10 match ids"
        else mapM runParserFromText $ map T.pack xs




scrapeMatches :: AppM ()
scrapeMatches = do
  logInfo $ "*** Scraping matches ***"

  doUpdateMatchIds
  matchIds <- query' GetMatchIds
  let matchIdsToUpdate = force $ VB.fromList $ L.sort . HM.keys $ HM.filter (== MatchFetchStatusUntried) matchIds

  logInfo $ "*** " <> (displayShow $ HM.size matchIds) <> " matchIds in the DB and " <> (displayShow $ VB.length matchIdsToUpdate) <> " matches need to be scraped ***"
  appEnv <- ask
  appState <- get
  liftIO $ withThreads appEnv (stackToIO' appEnv appState scrapeMatch) matchIdsToUpdate


matchPageUrl :: MatchId -> Text
matchPageUrl mid  = vooblyUrl <> "/match/view/" <> (T.pack . show $ matchIdToInt mid)

scrapeMatch :: MatchId -> AppM ()
scrapeMatch mid =
  case matchVooblyIssue mid of
    Just a -> do
      update' $ UpdateMatchId mid a
      logWarn $ "Voobly issue handled " <> displayShow a <> " for match " <> displayShow mid
    Nothing -> MC.catch (doScrapeMatch mid) handleMatchError
  where
    handleMatchError :: AppError -> AppM ()
    handleMatchError (AppErrorVooblyIssue t) = do
      logWarn $ "Caught AppErrorVooblyIssue when updating match " <> displayShow mid <> ": " <> displayShow t
      update' $ UpdateMatchId mid (MatchFetchStatusVooblyIssue t)

    handleMatchError e = do
      logError $ "Caught AppError when updating match " <> displayShow mid <> ": " <> displayShow e
      update' $ UpdateMatchId mid (MatchFetchStatusExceptionError e)








doScrapeMatch :: MatchId -> AppM ()
doScrapeMatch mid = do
  logDebug $ "Scraping match " <> (displayShow . matchPageUrl $ mid)
  t <- makeTextRequest $ matchPageUrl mid
  ladder <- extractMatchLadder t
  case ladder of
    Left l -> do
      update' $ UpdateMatchId mid (MatchFetchStatusUnsupportedLadder l)
      logDebug $ "Unsupported ladder " <> displayShow l
    Right l -> do
      date <- extractMatchDate t
      duration <- extractMatchDuration t
      mapName <- extractMatchMap t
      mapNumberOfPlayers <- extractMatchNumberOfPlayers mid t
      matchMods <- extractMatchMods t
      (winningTeam, players) <- extractMapPlayers mapNumberOfPlayers t
      let match = Match {
              matchId = mid
            , matchDate = date
            , matchDuration = duration
            , matchLadder = l
            , matchMap = mapName
            , matchMods = matchMods
            , matchPlayers = players
            , matchWinner = winningTeam
        }
      knownPlayers <- (flip mapM) players $ \p ->
        case p of
          MatchPlayer{} -> fmap isJust $ query' $ GetPlayer $ matchPlayerPlayerId p
          MatchPlayerError{} -> pure True
      if and knownPlayers
        then do

          update' $ UpdateMatch match
          update' $ UpdateMatchId mid MatchFetchStatusComplete
          logDebug $ "Inserted match with id " <> displayShow mid
          return ()

        else do
          now <- getCurrentTime
          update' $ UpdateMatchId mid (MatchFetchStatusMissingPlayer now)
          logWarn $ "Missing player in match " <> displayShow mid



extractMatchLadder :: Text -> AppM (Either Text Ladder)
extractMatchLadder t =
 case doRegexJustCaptureGroups (T.unpack t) "Ladder: <a href=\"[^\"]+\">([^<]+)</a>" of
  [x] ->
    case x of
      "RM - 1v1" -> pure $ Right LadderRm
      "RM - Team Games" -> pure $ Right LadderRmTeam
      _ -> pure $ Left (T.pack x)
  _ -> throwM $ AppErrorInvalidHtml "Expected to find one string for ladder"

extractMatchDate :: Text -> AppM UTCTime
extractMatchDate t =
 case doRegexJustCaptureGroups (T.unpack t) "<td style=\"[^\"]*\">Date Played:</td>\n<td style=\"[^\"]*\">([^<]+)</td>" of
  [x] -> parseTimeM True defaultTimeLocale "%e %B %Y - %l:%M %P" x
  _ -> throwM $ AppErrorInvalidHtml "Expected to find one date string for match date"

extractMatchDuration :: Text -> AppM DiffTime
extractMatchDuration t =
 case doRegexJustCaptureGroups (T.unpack t) "<td style=\"[^\"]*\" bgcolor=\"[^\"]*\">Duration:</td>\n<td style=\"[^\"]*\" bgcolor=\"[^\"]*\">([^<]+)</td>" of
  [x] -> do
    tod <- parseTimeM True defaultTimeLocale "%T" x
    return $ timeOfDayToTime tod
  _ -> throwM $ AppErrorInvalidHtml "Expected to find one time string for match duration"


extractMatchMap :: Text -> AppM Text
extractMatchMap t = do
  debugTextToFile t Nothing
  case doRegexJustCaptureGroups (T.unpack t) "<td style=\"[^\"]*\">Map:</td>\n<td style=\"[^\"]*\">([^<]+)</td>" of
    [x] -> return $ T.strip . T.pack $ x
    xs -> throwM $ AppErrorInvalidHtml $ "Expected to find one text string for match map but found " <> (utf8BuilderToText . displayShow $ xs)

extractMatchNumberOfPlayers :: MatchId -> Text -> AppM Int
extractMatchNumberOfPlayers mid t = do
  case matchOverrideNumberOfPlayers mid of
    Just i -> pure i
    Nothing ->
     case doRegexJustCaptureGroups (T.unpack t) "<td style=\"[^\"]*\">Players:</td>\n<td style=\"[^\"]*\">([^<]+)</td>" of
      [x] -> runParserFromText $ T.pack x
      _ -> throwM $ AppErrorInvalidHtml "Expected to find one int for match number of players"

extractMatchMods :: Text -> AppM [Text]
extractMatchMods t =
 case doRegexJustCaptureGroups (T.unpack t) "<td style=\"[^\"]*\" bgcolor=\"[^\"]*\">Game Mod:</td>\n<td style=\"[^\"]*\" bgcolor=\"[^\"]*\">([^<]+)</td>" of
  [x] -> return $ map T.strip $ T.split (== '|') . T.pack $ x
  _ -> throwM $ AppErrorInvalidHtml "Expected to find one text string for match mods"

-- we were checking expected players vs player tables found as a sanity check, but too many times Voobly just has the wrong number of players, so we are going to simply trust in their html (which is probably asking for trouble, but still...)
extractMapPlayers :: Int -> Text -> AppM (Team, [MatchPlayer])
extractMapPlayers _expected t = do
  (winT, loseT) <- findPlayerTables t
  let psFound = length winT + length loseT
  if  psFound > 1 && psFound < 9
    then do
      winners <- mapM (extractMatchPlayer True) winT
      losers <- mapM (extractMatchPlayer False) loseT
      case L.nub $ map matchPlayerTeam (filter (not . isMatchPlayerError) winners) of
        [] -> throwM $ AppErrorVooblyIssue "No winning team"
        [x] -> return (x, winners ++ losers)
        xs -> throwM $ AppErrorInvalidHtml $  "Expected to find exactly one winning team but got" <> (utf8BuilderToText . displayShow $ xs)
    else throwM $ AppErrorInvalidHtml $  "Expected to find between 1 and 8 player tables but found " <> (utf8BuilderToText . displayShow $ psFound)
    --else throwM $ AppErrorInvalidHtml $  "Expected to find exactly " <> (utf8BuilderToText . displayShow $ expected) <> " player tables, but found " <> (utf8BuilderToText . displayShow $ psFound)

treeToText :: Tree P.Token -> Text
treeToText = T.filter (/= '\n') .  TL.toStrict . P.renderTokens . P.tokensFromTree

isErrorComputerOrDeletedPlayer :: Tree P.Token -> AppM Bool
isErrorComputerOrDeletedPlayer t =
  case Safe.lastMay $ extractFromTree t (isTagOpen "a") of
    Nothing -> pure $ not . null $ doRegexJustCaptureGroups (T.unpack . treeToText $ t) "(Computer)"
    Just _ -> pure False

extractMatchPlayer :: Bool -> Tree P.Token -> AppM MatchPlayer
extractMatchPlayer isWinner t = do
  pNameTree <- playerNameTree
  isErr <- isErrorComputerOrDeletedPlayer pNameTree
  if isErr
    then pure $ MatchPlayerError (treeToText pNameTree)
    else do
      (_, playerId) <- extractNameAndIdFromToken pNameTree
      civId <- extractCivFromTree
      (oldRating, newRating, team) <- extractPlayerMatchRating

      return MatchPlayer {
          matchPlayerPlayerId = playerId
        , matchPlayerCiv = civId
        , matchPlayerPreRating = oldRating
        , matchPlayerPostRating = newRating
        , matchPlayerTeam = team
        , matchPlayerWon = isWinner
        }

  where

    extractPlayerMatchRating :: AppM (Int, Int, Team)
    extractPlayerMatchRating = do

      let reg =
            if isWinner
              then "New Rating: <b>([0-9]+)</b>Points: <b><span style=\"[^\"]*\">([0-9]+)</span></b> Team:<b>([0-9]+)</b>"
              else "Team: <b>([0-9]+)</b> Points:<b><span style=\"[^\"]*\">-?([0-9]+)</span></b> New Rating:<b>([0-9]+)</b>"
          normalise = if isWinner then id else reverse
      case normalise $ doRegexJustCaptureGroups (T.unpack . T.filter (/= '\n') . TL.toStrict . P.renderTokens . P.tokensFromTree $ t) reg of
        newRatingS:pointsChangeS:teamS:[] -> do
          newRating <- runParserFromText $ T.pack newRatingS
          pointsChange <- runParserFromText $ T.pack pointsChangeS
          teamI <- runParserFromText $ T.pack teamS
          let oldRating =
                if isWinner
                  then newRating - pointsChange
                  else newRating + pointsChange
          return (oldRating, newRating, Team teamI)


        xs -> throwM $ AppErrorInvalidHtml $  "Expected to find three capture groups in extractPlayerMatchRating, but got " <> (utf8BuilderToText . displayShow $ xs) <> " with regex: " <> (utf8BuilderToText . displayShow $ reg)




    playerNameTree :: AppM (Tree P.Token)
    playerNameTree =
      case extractFromTree t isPlayerNameTree of
        [x] -> return x
        _ -> throwM $ AppErrorInvalidHtml $  "Expected to find one player name td in extractMatchPlayer"
    isPlayerNameTree :: P.Token -> Bool
    isPlayerNameTree (P.TagOpen tag attrs) =
      let w = fromMaybe "" $ findAttributeValue "valign" attrs
          cp = fromMaybe "" $ findAttributeValue "align" attrs
      in tag == "td" && w == "bottom" && (cp == "right" || cp == "") && length attrs `elem` [1,2]
    isPlayerNameTree _ = False
    extractCivFromTree :: AppM CivilisationId
    extractCivFromTree = do
      case P.tokensFromForest $ extractFromTree t isCivImg of
        [P.TagSelfClose _ attrs] -> do
          let src = fromMaybe "" $ findAttributeValue "src" attrs
          case doRegexJustCaptureGroups (T.unpack src) "res/games/AOC/civs/([0-9]+)" of
            [civIdS] -> do
              civId <- fmap CivilisationId $ runParserFromText $ T.pack civIdS

              civ <- query' $ GetCivilisation civId
              case civ of
                Nothing ->
                  case L.find (\x -> civilisationId x == civId) defaultCivs of
                    Nothing -> throwM $ AppErrorMissingCiv $  "Civ not present in database or default list: " <> (utf8BuilderToText . displayShow $ civId)
                    Just c -> do
                      update' $ UpdateCivilisation c
                      return $ civId
                Just _ -> return civId

            _ -> throwM $ AppErrorInvalidHtml $  "Expected to find civ img int with regex in extractMatchPlayer"
        _ -> throwM $ AppErrorInvalidHtml $  "Expected to find civ img in extractMatchPlayer"


    isCivImg  :: P.Token -> Bool
    isCivImg (P.TagSelfClose tagName attrs) =
      let src = fromMaybe "" $ findAttributeValue "src" attrs
      in tagName == "img" && length (doRegex (T.unpack src) "res/games/AOC/civs") > 0
    isCivImg _ = False
findPlayerTables :: Text -> AppM ([Tree P.Token], [Tree P.Token])
findPlayerTables t = do
  mainTable <- extractMainPlayerTable t
  (winnerTable, loserTable) <- extractWinnerLoserTables mainTable
  winnerTables <- extractPlayerTables winnerTable
  loserTables <- extractPlayerTables loserTable
  return (winnerTables, loserTables)
  where
    extractWinnerLoserTables :: Tree P.Token -> AppM (Tree P.Token, Tree P.Token)
    extractWinnerLoserTables tr =
      case extractFromTree tr isWlTable of
        w:l:[] -> return (w, l)
        _ -> throwM $ AppErrorInvalidHtml $  "Expected to find one winner and one loser table"
    isWlTable :: P.Token -> Bool
    isWlTable (P.TagOpen tag attrs) =
      let w = fromMaybe "" $ findAttributeValue "valign" attrs
          cp = fromMaybe "" $ findAttributeValue "width" attrs
      in tag == "td" && w == "top" && cp == "50%" && length attrs == 2
    isWlTable _ = False
    extractPlayerTables :: Tree P.Token -> AppM [Tree P.Token]
    extractPlayerTables = pure . subForest -- there should only be the player tables at this point


extractMainPlayerTable :: Text -> AppM (Tree P.Token)
extractMainPlayerTable baseT = do
  t <- fmap (T.filter (/= '\n')) $ htmlTidy baseT

  let tokens = P.canonicalizeTokens $ P.parseTokens t
  forest <- tokensToForestM tokens
  case extractFromForest forest isMainPlayerTable  of
    [x] -> return x
    xs -> throwM $ AppErrorInvalidHtml $  "Expected to find one main player table" <> (utf8BuilderToText . displayShow $ xs)

  where
    isMainPlayerTable :: P.Token -> Bool
    isMainPlayerTable (P.TagOpen tag attrs) =
      let w = fromMaybe "" $ findAttributeValue "width" attrs
          cp = fromMaybe "" $ findAttributeValue "cellpadding" attrs
      in tag == "table" && w == "100%" && cp == "0" && length attrs == 2
    isMainPlayerTable _ = False

type LadderRow = (Text, PlayerId, Int, Int, Int)

updatePlayerLadders :: Ladder -> LadderRow -> AppM ()
updatePlayerLadders l (name, pid, rating, wins, loss) = do
  let p = Player pid name Set.empty Nothing
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
extractNameAndIdFromToken t = do
  case Safe.lastMay $ extractFromTree t (isTagOpen "a") of
    Nothing -> throwM $ AppErrorInvalidHtml "Expected at least one a tag in extractNameAndIdFromToken"
    Just aTree ->
      case flatten $ replaceSpanWithContentText aTree  of
        (P.TagOpen _ attrs):(P.ContentText name):[] -> do

          case findAttributeValue "href" attrs of
            Nothing -> throwM $ AppErrorInvalidHtml "Expected href attribute in extractNameAndIdFromToken"
            Just href -> do
              case Safe.lastMay $ T.split (== '/') href of
                Nothing -> throwM $ AppErrorInvalidHtml "Expected multiple parts to url in extractNameAndIdFromToken"
                Just tId -> return (name, PlayerId . T.strip $ tId)



        _ -> throwM $ AppErrorInvalidHtml "Expected open tag, contenttext for extractNameAndIdFromToken"
  where
    replaceSpanWithContentText :: Tree P.Token -> Tree P.Token
    replaceSpanWithContentText tr =
      case subForest tr of
        [(Node (P.TagOpen "span" _) spForest)] -> tr{subForest = spForest}
        _ -> tr


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

debugTextToFile :: Text -> Maybe String -> AppM ()
debugTextToFile t n =
  let name = fromMaybe "debug" n
  in BL.writeFile (runDir <> "/" <> name <> ".html") . BL.fromStrict . encodeUtf8 $ t

debugTreeToFile :: Tree P.Token -> Maybe String -> AppM ()
debugTreeToFile t n =
    debugTokensToFile (P.tokensFromTree t) n

debugForestToFile :: Forest P.Token -> Maybe String -> AppM ()
debugForestToFile t n =
    debugTokensToFile (P.tokensFromForest t) n

debugTokensToFile :: [P.Token] -> Maybe String -> AppM ()
debugTokensToFile t n =
  debugTextToFile (TL.toStrict . P.renderTokens $ t) n

makeHTMLTreesRequest :: Text -> AppM ([P.Token])
makeHTMLTreesRequest u = do
  baseReq <- parseRequest $ T.unpack $ u
  res <- makeRequest baseReq
  return $ P.canonicalizeTokens $ P.parseTokens $ decodeUtf8With ignore (BL.toStrict $ responseBody res)

makeTextRequest :: Text -> AppM Text
makeTextRequest u = do
  baseReq <- parseRequest $ T.unpack $ u
  res <- makeRequest baseReq
  return $ decodeUtf8With ignore (BL.toStrict $ responseBody res)



tokensToForestM :: [P.Token] -> AppM (Forest P.Token)
tokensToForestM ts =
    case P.tokensToForest ts of
      Left e -> throwM $ AppErrorInvalidHtml $ utf8BuilderToText . displayShow $ e
      Right a -> return a

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
          -- this is to fix the buggy html
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


htmlTidy :: Text -> AppM Text
htmlTidy t = do
  (_exit, out, _err) <- liftIO $ SP.readProcessWithExitCode "tidy" ["--force-output", "yes", "--quiet", "yes"] (T.unpack t)
  --logDebug $ displayShow (exit, out, err)
  return $ T.pack out




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






{-
match specific overrides
-}

noWinningTeamIds :: [Int]
noWinningTeamIds = [
    16212164
  , 16220337
  , 16220355
  , 16221436
  , 16225891
  , 16226590

  ]


matchVooblyIssue :: MatchId -> Maybe MatchFetchStatus
matchVooblyIssue (MatchId i) =
  if i `elem` noWinningTeamIds
    then Just $ MatchFetchStatusVooblyIssue "No winning team"
    else Nothing


matchNumberOfPlayersOverride :: HM.HashMap MatchId Int
matchNumberOfPlayersOverride = HM.fromList [
    (MatchId 16210602, 3)
  , (MatchId 16213743, 3)
  , (MatchId 16220499, 2)
  , (MatchId 16220547, 2)
  ]


matchOverrideNumberOfPlayers :: MatchId -> Maybe Int
matchOverrideNumberOfPlayers mid = HM.lookup mid matchNumberOfPlayersOverride