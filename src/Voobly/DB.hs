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

newtype PlayerId = PlayerId {playerIdToText :: Text} deriving (Eq, Ord, Show)
newtype Team = Team {teamToInt :: Int} deriving (Eq, Ord, Show)
newtype CivilisationId = CivilisationId {civilisationIdToInt :: Int} deriving (Eq, Ord, Show)
newtype MatchId = MatchId {matchIdToInt :: Int} deriving (Eq, Ord, Show, Hashable, NFData)

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
  , (46, "VooblyCivError")
  ]


data Player = Player {
  playerId :: PlayerId,
  playerName :: Text,
  playerMatchIds :: !(Set.Set MatchId),
  playerLastCompletedUpdate :: Maybe UTCTime
} deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

ladderId :: Ladder -> Int
ladderId LadderRm = 131
ladderId LadderRmTeam = 132

data PlayerLadder = PlayerLadder {
  playerLadderPlayerId :: PlayerId,
  playerLadderLadder :: Ladder,
  playerLadderRating :: Int,
  playerLadderWins  :: Int,
  playerLadderLoss :: Int
} deriving (Eq, Ord, Show)


data Match = Match {
  matchId :: MatchId,
  matchDate :: UTCTime,
  matchDuration :: DiffTime,
  matchLadder :: Ladder,
  matchMap :: Text,
  matchMods :: [Text],
  matchPlayers :: [MatchPlayer],
  matchWinner :: Team
} deriving (Eq, Ord, Show)

data Civilisation = Civilisation {
  civilisationId :: CivilisationId,
  civilisationName :: Text
} deriving (Eq, Ord, Show)

data MatchPlayer = MatchPlayer {
  matchPlayerPlayerId :: PlayerId,
  matchPlayerCiv :: CivilisationId,
  matchPlayerPreRating :: Int,
  matchPlayerPostRating :: Int,
  matchPlayerTeam :: Team,
  matchPlayerWon :: Bool
} | MatchPlayerError Text deriving (Eq, Ord, Show)

data PlayerLadderProgress = PlayerLadderProgress {
  playerLadderProgressLadder :: Ladder
, playerLadderProgressLastPageHandled :: Maybe Int
, playerLadderProgressLastCompleted :: Maybe UTCTime
} deriving (Eq, Ord, Show)


data MatchFetchStatus =
    MatchFetchStatusUntried
  | MatchFetchStatusComplete
  | MatchFetchStatusUnsupportedLadder Text
  | MatchFetchStatusMissingPlayer UTCTime
  deriving (Eq, Ord, Show)

instance NFData MatchFetchStatus where
  rnf MatchFetchStatusUntried = ()
  rnf MatchFetchStatusComplete = ()
  rnf (MatchFetchStatusUnsupportedLadder t) = rnf t
  rnf (MatchFetchStatusMissingPlayer t) = rnf t

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

makeSimpleIxSet "PlayerSet" ''Player ['playerId, 'playerLastCompletedUpdate]
makeSimpleIxSet "PlayerLadderSet" ''PlayerLadder ['playerLadderPlayerId, 'playerLadderLadder, 'playerLadderUniqIdx]
makeSimpleIxSet "CivilisationSet" ''Civilisation ['civilisationId]
makeSimpleIxSet "MatchSet" ''Match ['matchId, 'matchLadder]
makeSimpleIxSet "PlayerLadderProgressSet" ''PlayerLadderProgress ['playerLadderProgressLadder]



data DB = DB {
  _dbCookies :: [Cookie]
, _dbPlayers :: PlayerSet
, _dbPlayerLadders :: PlayerLadderSet
, _dbCivilisations :: CivilisationSet
, _dbMatches :: MatchSet
, _dbPlayerLadderProgress :: PlayerLadderProgressSet
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
      let newP = p{playerMatchIds = force $ Set.union (playerMatchIds p) (Set.fromList matchIds)}
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


updateMatchIds ::  Update DB ()
updateMatchIds = do
  db <- get
  let allMatchIds = Set.unions (map playerMatchIds $ IxSet.toList (_dbPlayers db))
      updatedMap = force $ Set.foldr' (insertDefaultHMIfAbsent MatchFetchStatusUntried) (_dbMatchIds db) $ allMatchIds
  modify (L.set dbMatchIds updatedMap)

getMatchIds :: Query DB (HM.HashMap MatchId MatchFetchStatus)
getMatchIds = L.view dbMatchIds <$> ask


updateMatchId :: MatchId -> MatchFetchStatus -> Update DB ()
updateMatchId a b = modify (over dbMatchIds (\m -> HM.insert a b m))

getCivilisation :: CivilisationId -> Query DB (Maybe Civilisation)
getCivilisation pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbCivilisations <$> ask

updateCivilisation :: Civilisation -> Update DB ()
updateCivilisation a = modify (over dbCivilisations (IxSet.updateIx (civilisationId a) a))

updateMatch :: Match -> Update DB ()
updateMatch a = modify (over dbMatches (IxSet.updateIx (matchId a) a))







instance Csv.ToField MatchId where
  toField = Csv.toField . matchIdToInt

instance Csv.ToField CivilisationId where
  toField = Csv.toField . civilisationIdToInt

instance Csv.ToField Team where
  toField = Csv.toField . teamToInt

instance Csv.ToField PlayerId where
  toField = Csv.toField . playerIdToText

instance Csv.ToField [Text] where
  toField = Csv.toField . T.intercalate ", "

instance Csv.ToField Ladder where
  toField LadderRm = Csv.toField $ ("RM - 1v1" :: Text)
  toField LadderRmTeam = Csv.toField $ ("RM - Team" :: Text)

instance Csv.ToField DiffTime where
  toField dt = Csv.toField $ ((round  (fromIntegral (diffTimeToPicoseconds dt) / (10^(12 :: Integer) :: Double))) :: Int)

instance Csv.ToField UTCTime where
  toField dt = Csv.toField $ formatTime defaultTimeLocale rfc822DateFormat dt

instance Csv.ToField Bool where
  toField True = "1"
  toField False = "0"

instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (HM.HashMap a b) where
  getCopy = contain $ fmap HM.fromList safeGet
  putCopy = contain . safePut . HM.toList


$(deriveSafeCopy 0 'base ''Cookie)
$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''PlayerId)
$(deriveSafeCopy 1 'extension ''Player)
$(deriveSafeCopy 0 'base ''Player_v0)
$(deriveSafeCopy 0 'base ''Ladder)
$(deriveSafeCopy 0 'base ''PlayerLadder)
$(deriveSafeCopy 0 'base ''Team)
$(deriveSafeCopy 0 'base ''CivilisationId)
$(deriveSafeCopy 0 'base ''Match)
$(deriveSafeCopy 0 'base ''Civilisation)
$(deriveSafeCopy 0 'base ''MatchPlayer)
$(deriveSafeCopy 0 'base ''MatchId)
$(deriveSafeCopy 0 'base ''PlayerLadderProgress)
$(deriveSafeCopy 0 'base ''MatchFetchStatus)

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
  , 'updateMatchIds
  , 'updateMatchId
  , 'getCivilisation
  , 'updateMatch
  , 'updateCivilisation
  , 'addNewMatchIds
  ])


