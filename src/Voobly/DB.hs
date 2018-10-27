{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Voobly.DB where


import RIO
import RIO.Time
import qualified RIO.Text as T
import Voobly.TH
import Data.Acid
import Data.SafeCopy
import Network.HTTP.Client
import Control.Monad.State
import qualified Control.Lens as L
import qualified Data.IxSet.Typed as IxSet
import qualified RIO.HashMap as HM
import Data.Hashable
import qualified Data.Csv as Csv
import qualified RIO.Vector.Boxed as VB
import qualified RIO.Set as Set
import Data.Aeson

newtype PlayerId = PlayerId {playerIdToInt :: Int} deriving (Eq, Ord, Show, Generic)

instance Migrate PlayerId where
  type MigrateFrom PlayerId = PlayerId_v0
  migrate (PlayerId_v0 t) =
    case Csv.runParser . Csv.parseField $ encodeUtf8 t of
      Left err -> error $ "In migration " ++ err
      Right i -> PlayerId i
newtype PlayerId_v0 = PlayerId_v0 {v0_playerIdToText :: Text} deriving (Eq, Ord, Show, Generic)
newtype Team = Team {teamToInt :: Int} deriving (Eq, Ord, Show, Generic)
newtype CivilisationId = CivilisationId {civilisationIdToInt :: Int} deriving (Eq, Ord, Show, Generic)
newtype MatchId = MatchId {matchIdToInt :: Int} deriving (Eq, Ord, Show, Hashable, NFData, Generic)

defaultCivs :: [Civilisation]
defaultCivs = map (\(a,b) -> Civilisation (CivilisationId a) b) $ filter (\x -> (T.length . snd $ x) > 0) defaultCivTups

defaultCivTups :: [(Int, Text)]
defaultCivTups = [
    (1, "Britons")
  , (2, "Franks")
  , (3, "Goths")
  , (4, "Teutons")
  , (5, "Japanese")
  , (6, "Chinese")
  , (7, "Byzantines")
  , (8, "Persians")
  , (9, "Saracens")
  , (10, "Turks")
  , (11, "Vikings")
  , (12, "Mongols")
  , (13, "Celts")
  , (14, "Spanish")
  , (15, "Aztecs")
  , (16, "Mayans")
  , (17, "Huns")
  , (18, "Koreans")
  , (19, "Italians")
  , (20, "Indians")
  , (21, "Incas")
  , (22, "Magyars")
  , (23, "Slavs")
  , (24, "Portuguese")
  , (25, "Ethiopian")
  , (26, "Malian")
  , (27, "Berbers")
  , (28, "Khmer")
  , (29, "Malay")
  , (30, "Burmese")
  , (31, "Vietnamese")
  , (32, "VooblyCivError")
  , (33, "VooblyCivError")
  , (34, "VooblyCivError")
  , (36, "VooblyCivError")
  , (35, "VooblyCivError")
  , (37, "VooblyCivError")
  , (38, "VooblyCivError")
  , (39, "VooblyCivError")
  , (40, "VooblyCivError")
  , (41, "VooblyCivError")
  , (46, "VooblyCivError")
  , (47, "VooblyCivError")
  ]


data Player = Player {
  playerId :: !PlayerId,
  playerName :: !Text,
  playerMatchIds :: !(Set.Set MatchId),
  playerLastCompletedUpdate :: !(Maybe UTCTime)
} deriving (Eq, Ord, Show, Generic)

instance Migrate Player where
  type MigrateFrom Player = Player_v0
  migrate Player_v0{..} = Player {
    playerId = v0_playerId
  , playerName = v0_playerName
  , playerMatchIds = Set.fromList . VB.toList $ v0_playerMatchIds
  , playerLastCompletedUpdate = v0_playerLastCompletedUpdate

  }

data Player_v0 = Player_v0 {
  v0_playerId :: PlayerId,
  v0_playerName :: Text,
  v0_playerMatchIds :: !(Vector MatchId),
  v0_playerLastCompletedUpdate :: Maybe UTCTime
} deriving (Eq, Ord, Show)

data Ladder =
    LadderRm
  | LadderRmTeam
  | LadderDm
  | LadderDmTeam
  | LadderMatchStatsOnly
  deriving (Eq, Ord, Show, Generic)

ladderId :: Ladder -> Int
ladderId LadderRm = 131
ladderId LadderRmTeam = 132
ladderId LadderDm = 162
ladderId LadderDmTeam = 163
ladderId LadderMatchStatsOnly = 171

data PlayerLadder = PlayerLadder {
  playerLadderPlayerId :: !PlayerId,
  playerLadderLadder :: !Ladder,
  playerLadderRating :: !Int,
  playerLadderWins  :: !Int,
  playerLadderLoss :: !Int
} deriving (Eq, Ord, Show)


data Match = Match {
  matchId :: !MatchId,
  matchDate :: !UTCTime,
  matchDuration :: !DiffTime,
  matchLadder :: !Ladder,
  matchMap :: !Text,
  matchMods :: ![Text],
  matchPlayers :: ![MatchPlayer],
  matchWinner :: !Team
} deriving (Eq, Ord, Show, Generic)

instance NFData Ladder
instance NFData Team
instance NFData PlayerId
instance NFData MatchPlayer
instance NFData CivilisationId
instance NFData MatchFetchStatus
instance NFData AppError
instance NFData Match
instance NFData Player
instance NFData Recording

instance ToJSON Match
instance ToJSON MatchId
instance ToJSON Ladder
instance ToJSON MatchPlayer
instance ToJSON PlayerId
instance ToJSON CivilisationId
instance ToJSON Team
instance ToJSON Recording
instance FromJSON Match
instance FromJSON MatchId
instance FromJSON Ladder
instance FromJSON MatchPlayer
instance FromJSON PlayerId
instance FromJSON CivilisationId
instance FromJSON Team
instance FromJSON Recording





data Civilisation = Civilisation {
  civilisationId :: CivilisationId,
  civilisationName :: Text
} deriving (Eq, Ord, Show)

data Recording = Recording {
  recordingUrl :: !Text,
  recordingLocal :: !(Maybe FilePath),
  recordingNoLongerExists :: Bool
} deriving (Eq, Ord, Show, Generic)

data MatchPlayer = MatchPlayer {
  matchPlayerPlayerId :: !PlayerId,
  matchPlayerCiv :: !CivilisationId,
  matchPlayerPreRating :: !Int,
  matchPlayerPostRating :: !Int,
  matchPlayerTeam :: !Team,
  matchPlayerWon :: !Bool,
  matchPlayerRecording :: !(Maybe Recording)
} | MatchPlayerError !Text deriving (Eq, Ord, Show, Generic)

instance Migrate MatchPlayer where
  type MigrateFrom MatchPlayer = MatchPlayer_v1
  migrate MatchPlayer_v1{..} = MatchPlayer{
     matchPlayerPlayerId   = v1_matchPlayerPlayerId
   , matchPlayerCiv        = v1_matchPlayerCiv
   , matchPlayerPreRating  = v1_matchPlayerPreRating
   , matchPlayerPostRating = v1_matchPlayerPostRating
   , matchPlayerTeam       = v1_matchPlayerTeam
   , matchPlayerWon        = v1_matchPlayerWon
   , matchPlayerRecording  = fmap (\x -> Recording x Nothing False) v1_matchPlayerRecording
  }
  migrate (MatchPlayerError_v1 t) = MatchPlayerError t

data MatchPlayer_v1 = MatchPlayer_v1 {
  v1_matchPlayerPlayerId :: !PlayerId,
  v1_matchPlayerCiv :: !CivilisationId,
  v1_matchPlayerPreRating :: !Int,
  v1_matchPlayerPostRating :: !Int,
  v1_matchPlayerTeam :: !Team,
  v1_matchPlayerWon :: !Bool,
  v1_matchPlayerRecording :: !(Maybe Text)
} | MatchPlayerError_v1 !Text deriving (Eq, Ord, Show, Generic)

instance Migrate MatchPlayer_v1 where
  type MigrateFrom MatchPlayer_v1 = MatchPlayer_v0
  migrate MatchPlayer_v0{..} = MatchPlayer_v1{
     v1_matchPlayerPlayerId   = v0_matchPlayerPlayerId
   , v1_matchPlayerCiv        = v0_matchPlayerCiv
   , v1_matchPlayerPreRating  = v0_matchPlayerPreRating
   , v1_matchPlayerPostRating = v0_matchPlayerPostRating
   , v1_matchPlayerTeam       = v0_matchPlayerTeam
   , v1_matchPlayerWon        = v0_matchPlayerWon
   , v1_matchPlayerRecording  = Nothing
  }
  migrate (MatchPlayerError_v0 t) = MatchPlayerError_v1 t

data MatchPlayer_v0 = MatchPlayer_v0 {
  v0_matchPlayerPlayerId :: !PlayerId,
  v0_matchPlayerCiv :: !CivilisationId,
  v0_matchPlayerPreRating :: !Int,
  v0_matchPlayerPostRating :: !Int,
  v0_matchPlayerTeam :: !Team,
  v0_matchPlayerWon :: !Bool
} | MatchPlayerError_v0 !Text deriving (Eq, Ord, Show, Generic)

isMatchPlayerError :: MatchPlayer -> Bool
isMatchPlayerError (MatchPlayerError _) = True
isMatchPlayerError _ = False

data PlayerLadderProgress = PlayerLadderProgress {
  playerLadderProgressLadder :: Ladder
, playerLadderProgressLastPageHandled :: Maybe Int
, playerLadderProgressLastCompleted :: Maybe UTCTime
} deriving (Eq, Ord, Show)

data AppError =
    AppErrorCouldntLogIn Text
  | AppErrorInvalidHtml Text
  | AppErrorParserError Text
  | AppErrorNotFound Text
  | AppErrorCommandFailure Text
  | AppErrorMissingCiv Text
  | AppErrorDBError Text
  | AppErrorVooblyIssue Text
  | AppErrorMatchPageNotFound
  | AppErrorInvalidZip Text
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Exception AppError


data MatchFetchStatus =
    MatchFetchStatusUntried
  | MatchFetchStatusComplete
  | MatchFetchStatusUnsupportedLadder !Text
  | MatchFetchStatusMissingPlayer !UTCTime
  | MatchFetchStatusVooblyIssue !Text
  | MatchFetchStatusExceptionError !AppError
  | MatchFetchStatusMatchPageNotFound
  deriving (Eq, Ord, Show, Generic)

maybeUnsupportedLadder :: MatchFetchStatus -> Maybe Text
maybeUnsupportedLadder (MatchFetchStatusUnsupportedLadder t) = Just t
maybeUnsupportedLadder _ = Nothing

isMatchFetchStatusUntried :: MatchFetchStatus -> Bool
isMatchFetchStatusUntried (MatchFetchStatusUntried) = True
isMatchFetchStatusUntried _ = False

isMatchFetchStatusComplete :: MatchFetchStatus -> Bool
isMatchFetchStatusComplete (MatchFetchStatusComplete) = True
isMatchFetchStatusComplete _ = False

isMatchFetchStatusUnsupportedLadder :: MatchFetchStatus -> Bool
isMatchFetchStatusUnsupportedLadder (MatchFetchStatusUnsupportedLadder _) = True
isMatchFetchStatusUnsupportedLadder _ = False

isMatchFetchStatusMissingPlayer :: MatchFetchStatus -> Bool
isMatchFetchStatusMissingPlayer (MatchFetchStatusMissingPlayer _) = True
isMatchFetchStatusMissingPlayer _ = False

isMatchFetchStatusVooblyIssue :: MatchFetchStatus -> Bool
isMatchFetchStatusVooblyIssue (MatchFetchStatusVooblyIssue _) = True
isMatchFetchStatusVooblyIssue _ = False

isMatchFetchStatusExceptionError :: MatchFetchStatus -> Bool
isMatchFetchStatusExceptionError (MatchFetchStatusExceptionError _) = True
isMatchFetchStatusExceptionError _ = False

isMatchFetchStatusMatchPageNotFound :: MatchFetchStatus -> Bool
isMatchFetchStatusMatchPageNotFound (MatchFetchStatusMatchPageNotFound) = True
isMatchFetchStatusMatchPageNotFound _ = False



instance Hashable MatchFetchStatus where
  hash = hash.show
  hashWithSalt i a = hashWithSalt i (show a)

instance Hashable CivilisationId where
  hash = hash.show
  hashWithSalt i a = hashWithSalt i (show a)

defaultPlayerLadderProgress :: Ladder -> PlayerLadderProgress
defaultPlayerLadderProgress l = PlayerLadderProgress l Nothing Nothing

playerLadderUniqIdx :: PlayerLadder -> (PlayerId, Ladder)
playerLadderUniqIdx a = (playerLadderPlayerId a, playerLadderLadder a)

newtype MissingLocalRecording = MissingLocalRecording {missingLocalRecordingBool :: Bool}  deriving (Eq, Ord, Show, Generic)

missingLocalRecordingIx :: Match -> MissingLocalRecording
missingLocalRecordingIx Match{..} = MissingLocalRecording $ or $ map (playerMissingLocalRecordingAndCanBeTriedForOne) matchPlayers

playerMissingLocalRecordingAndCanBeTriedForOne :: MatchPlayer -> Bool
playerMissingLocalRecordingAndCanBeTriedForOne MatchPlayer{..} =
  case matchPlayerRecording of
    Nothing -> False
    Just Recording{..} -> isNothing recordingLocal && not recordingNoLongerExists
playerMissingLocalRecordingAndCanBeTriedForOne MatchPlayerError{} = False

playerHasLocalRecording :: MatchPlayer -> Bool
playerHasLocalRecording MatchPlayer{..} =
  case matchPlayerRecording of
    Nothing -> False
    Just Recording{..} -> isJust recordingLocal
playerHasLocalRecording MatchPlayerError{} = False


makeSimpleIxSet "PlayerSet" ''Player ['playerId, 'playerLastCompletedUpdate]
makeSimpleIxSet "PlayerLadderSet" ''PlayerLadder ['playerLadderPlayerId, 'playerLadderLadder, 'playerLadderUniqIdx]
makeSimpleIxSet "CivilisationSet" ''Civilisation ['civilisationId]
makeSimpleIxSet "MatchSet" ''Match ['matchId, 'matchLadder, 'missingLocalRecordingIx]
makeSimpleIxSet "PlayerLadderProgressSet" ''PlayerLadderProgress ['playerLadderProgressLadder]



data DB = DB {
  _dbCookies :: ![Cookie]
, _dbPlayers :: !PlayerSet
, _dbPlayerLadders :: !PlayerLadderSet
, _dbCivilisations :: !CivilisationSet
, _dbMatches :: !MatchSet
, _dbPlayerLadderProgress :: !PlayerLadderProgressSet
, _dbMatchIds :: !(HM.HashMap MatchId MatchFetchStatus)
}

L.makeLenses ''DB


emptyDb :: DB
emptyDb = DB {
  _dbCookies = []
, _dbPlayers = IxSet.empty
, _dbPlayerLadders = IxSet.empty
, _dbCivilisations = IxSet.fromList defaultCivs
, _dbMatches = IxSet.empty
, _dbPlayerLadderProgress = IxSet.empty
, _dbMatchIds = HM.empty
}

updateDB :: DB -> Update DB ()
updateDB db = put db

getDB :: Query DB DB
getDB = ask


updateCookies :: [Cookie] -> Update DB ()
updateCookies cookies = modify (L.set dbCookies cookies)

getCookies :: Query DB [Cookie]
getCookies = L.view dbCookies <$> ask

getPlayer :: PlayerId -> Query DB (Maybe Player)
getPlayer pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbPlayers <$> ask

getPlayerLadder :: PlayerId -> Ladder -> Query DB (Maybe PlayerLadder)
getPlayerLadder pid ladder = (IxSet.getOne . IxSet.getEQ pid . IxSet.getEQ ladder) <$> L.view dbPlayerLadders <$> ask

updatePlayer :: Player -> Update DB ()
updatePlayer a = modify (over dbPlayers (IxSet.updateIx (playerId a) a))


insertIfAbsent :: (Eq a) => a -> Vector a -> Vector a
insertIfAbsent a v =
  if a `VB.notElem` v
    then VB.cons a v
    else v

addNewMatchIds :: PlayerId -> [MatchId] -> Update DB (Maybe Text)
addNewMatchIds pid matchIds = do
  mp <- liftQuery $ getPlayer pid
  case mp of
    Nothing -> return $ Just $ "Could not find player with id " <> (T.pack . show $ pid) <> " when updating match ids"
    Just p -> do
      let newP = p{playerMatchIds = Set.union (playerMatchIds p) (Set.fromList matchIds)}
      updatePlayer newP
      return Nothing


updatePlayerLadder :: PlayerLadder -> Update DB ()
updatePlayerLadder a = modify (over dbPlayerLadders (IxSet.updateIx (playerLadderUniqIdx a) a))

getPlayerLadderProgress :: Ladder -> Query DB (Maybe PlayerLadderProgress)
getPlayerLadderProgress pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbPlayerLadderProgress <$> ask


updatePlayerLadderProgress :: PlayerLadderProgress -> Update DB ()
updatePlayerLadderProgress a = modify (over dbPlayerLadderProgress (IxSet.updateIx (playerLadderProgressLadder a) a))

insertDefaultHMIfAbsent :: (Eq a, Hashable a) => b -> a -> HM.HashMap a b -> HM.HashMap a b
insertDefaultHMIfAbsent b a m =
  case HM.lookup a m of
    Nothing -> HM.insert a b m
    Just _ -> m

displayShowT ::  Show a => a -> Text
displayShowT = utf8BuilderToText . displayShow


updateMatchIds ::  Update DB ()
updateMatchIds = do
  db <- get
  let allMatchIds = Set.unions (map playerMatchIds $ IxSet.toList (_dbPlayers db))
      updatedMap = Set.foldr' (insertDefaultHMIfAbsent MatchFetchStatusUntried) (_dbMatchIds db) $ allMatchIds
  modify (L.set dbMatchIds updatedMap)

getMatchIds :: Query DB (HM.HashMap MatchId MatchFetchStatus)
getMatchIds = L.view dbMatchIds <$> ask


updateMatchId :: MatchId -> MatchFetchStatus -> Update DB ()
updateMatchId !a !b = do
  db <- get
  let updatedMap = HM.insert a b (_dbMatchIds db)
  modify (L.set dbMatchIds updatedMap)


getCivilisation :: CivilisationId -> Query DB (Maybe Civilisation)
getCivilisation pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbCivilisations <$> ask

updateCivilisation :: Civilisation -> Update DB ()
updateCivilisation a = modify (over dbCivilisations (IxSet.updateIx (civilisationId a) a))

getMatch :: MatchId -> Query DB (Maybe Match)
getMatch pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbMatches <$> ask

updateMatchPlayer :: MatchId -> MatchPlayer -> Update DB (Maybe Text)
updateMatchPlayer mid mpl = do
  match <- liftQuery $ getMatch mid
  case match of
    Nothing -> return . Just $ "Match not found for id " <> displayShowT mid
    Just m -> do
      case replaceMatchPlayer m of
        Left err -> return . Just $ err
        Right newM -> do
          modify'  (over dbMatches ((IxSet.updateIx (matchId newM) newM)))
          return Nothing



  where
    replaceMatchPlayer :: Match -> Either Text Match
    replaceMatchPlayer m = do
      let p = \x -> case x of mx@MatchPlayer{} -> matchPlayerPlayerId mpl ==  matchPlayerPlayerId mx; _ -> False
      case filter p $ matchPlayers m of
        [_] -> pure $ m{matchPlayers = mpl : filter (not .p) (matchPlayers m)}
        [] -> Left $ "Match " <> displayShowT (matchId m) <> " did not contain player with id " <> displayShowT (matchPlayerPlayerId mpl)
        _ -> Left $ "Match " <> displayShowT (matchId m) <> " contained multiple players matching " <> displayShowT (matchPlayerPlayerId mpl)

updateMatch :: Match -> Update DB ()
updateMatch !a = do
  modify'  (over dbMatches ((IxSet.updateIx (matchId a) a)))
  updateMatchId (matchId a) MatchFetchStatusComplete


{-updateMatch !a = do
  db <- get
  let s = _dbMatches db
      !b = IxSet.getOne . IxSet.getEQ (matchId a ) $ s
  modify'  (over dbMatches (IxSet.getGT (MatchId 0)))
  return b-}


deleteMatches :: Update DB ()
deleteMatches = modify (\db ->

  L.set dbMatches IxSet.empty db)




instance Csv.ToField MatchId where
  toField = Csv.toField . matchIdToInt

instance Csv.ToField CivilisationId where
  toField = Csv.toField . civilisationIdToInt

instance Csv.ToField Team where
  toField = Csv.toField . teamToInt

instance Csv.ToField PlayerId where
  toField = Csv.toField . playerIdToInt

instance Csv.ToField [Text] where
  toField = Csv.toField . T.intercalate ", "

instance Csv.ToField Ladder where
  toField LadderRm = Csv.toField $ ("RM - 1v1" :: Text)
  toField LadderRmTeam = Csv.toField $ ("RM - Team" :: Text)
  toField LadderDm = Csv.toField $ ("DM - 1v1" :: Text)
  toField LadderDmTeam = Csv.toField $ ("DM - Team" :: Text)
  toField LadderMatchStatsOnly = Csv.toField $ ("Match Stats Only" :: Text)

instance Csv.ToField DiffTime where
  toField dt = Csv.toField $ ((round  (fromIntegral (diffTimeToPicoseconds dt) / (10^(12 :: Integer) :: Double))) :: Int)

instance Csv.ToField UTCTime where
  toField dt = Csv.toField $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) dt

instance Csv.ToField Bool where
  toField True = "1"
  toField False = "0"

instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (HM.HashMap a b) where
  getCopy = contain $ fmap HM.fromList safeGet
  putCopy = contain . safePut . HM.toList


$(deriveSafeCopy 0 'base ''Cookie)
$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 1 'extension ''PlayerId)
$(deriveSafeCopy 0 'base ''PlayerId_v0)
$(deriveSafeCopy 1 'extension ''Player)
$(deriveSafeCopy 0 'base ''Player_v0)
$(deriveSafeCopy 0 'base ''Ladder)
$(deriveSafeCopy 0 'base ''PlayerLadder)
$(deriveSafeCopy 0 'base ''Team)
$(deriveSafeCopy 0 'base ''CivilisationId)
$(deriveSafeCopy 0 'base ''Match)
$(deriveSafeCopy 0 'base ''Civilisation)
$(deriveSafeCopy 2 'extension ''MatchPlayer)
$(deriveSafeCopy 1 'extension ''MatchPlayer_v1)
$(deriveSafeCopy 0 'base ''MatchPlayer_v0)
$(deriveSafeCopy 0 'base ''MatchId)
$(deriveSafeCopy 0 'base ''PlayerLadderProgress)
$(deriveSafeCopy 0 'base ''MatchFetchStatus)
$(deriveSafeCopy 0 'base ''AppError)
$(deriveSafeCopy 0 'base ''Recording)

$(makeAcidic ''DB [
  'updateDB,
  'getDB,
  'updateCookies,
  'getCookies,
  'getPlayer
  , 'updatePlayer
  , 'getPlayerLadder
  , 'updatePlayerLadder
  , 'getPlayerLadderProgress
  , 'updatePlayerLadderProgress
  , 'getMatchIds
  , 'getMatch
  , 'updateMatchIds
  , 'updateMatchId
  , 'getCivilisation
  , 'updateMatch
  , 'updateCivilisation
  , 'addNewMatchIds
  , 'deleteMatches
  , 'updateMatchPlayer
  ])


