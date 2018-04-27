{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Voobly.DB where


import RIO
import RIO.Time
import Voobly.TH
import Data.Acid
import Data.SafeCopy
import Network.HTTP.Client
import Control.Monad.State
import qualified Control.Lens as L
import qualified Data.IxSet.Typed as IxSet
import qualified RIO.HashMap as HM

newtype PlayerId = PlayerId {playerIdToText :: Text} deriving (Eq, Ord, Show)
newtype Team = Team Int deriving (Eq, Ord, Show)
newtype CivilisationId = CivilisationId Int deriving (Eq, Ord, Show)
newtype MatchId = MatchId Int deriving (Eq, Ord, Show, Hashable)



data Player = Player {
  playerId :: PlayerId,
  playerName :: Text,
  playerMatchIds :: Vector MatchId,
  playerLastCompletedUpdate :: Maybe UTCTime
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
  matchPlayerTeam :: Team
} deriving (Eq, Ord, Show)

data PlayerLadderProgress = PlayerLadderProgress {
  playerLadderProgressLadder :: Ladder
, playerLadderProgressLastPageHandled :: Maybe Int
, playerLadderProgressLastCompleted :: Maybe UTCTime
} deriving (Eq, Ord, Show)


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
, _dbMatchIds :: HM.HashMap MatchId Bool
}

L.makeLenses ''DB


emptyDb :: DB
emptyDb = DB {
  _dbCookies = []
, _dbPlayers = IxSet.empty
, _dbPlayerLadders = IxSet.empty
, _dbCivilisations = IxSet.empty
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

updatePlayerLadder :: PlayerLadder -> Update DB ()
updatePlayerLadder a = modify (over dbPlayerLadders (IxSet.updateIx (playerLadderUniqIdx a) a))

getPlayerLadderProgress :: Ladder -> Query DB (Maybe PlayerLadderProgress)
getPlayerLadderProgress pid = (IxSet.getOne . IxSet.getEQ pid) <$> L.view dbPlayerLadderProgress <$> ask


updatePlayerLadderProgress :: PlayerLadderProgress -> Update DB ()
updatePlayerLadderProgress a = modify (over dbPlayerLadderProgress (IxSet.updateIx (playerLadderProgressLadder a) a))

updateMatchIds :: (HM.HashMap MatchId Bool) -> Update DB ()
updateMatchIds cookies = modify (L.set dbMatchIds cookies)

getMatchIds :: Query DB (HM.HashMap MatchId Bool)
getMatchIds = L.view dbMatchIds <$> ask


instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (HM.HashMap a b) where
  getCopy = contain $ fmap HM.fromList safeGet
  putCopy = contain . safePut . HM.toList


$(deriveSafeCopy 0 'base ''Cookie)
$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''PlayerId)
$(deriveSafeCopy 0 'base ''Player)
$(deriveSafeCopy 0 'base ''Ladder)
$(deriveSafeCopy 0 'base ''PlayerLadder)
$(deriveSafeCopy 0 'base ''Team)
$(deriveSafeCopy 0 'base ''CivilisationId)
$(deriveSafeCopy 0 'base ''Match)
$(deriveSafeCopy 0 'base ''Civilisation)
$(deriveSafeCopy 0 'base ''MatchPlayer)
$(deriveSafeCopy 0 'base ''MatchId)
$(deriveSafeCopy 0 'base ''PlayerLadderProgress)

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
  ])


