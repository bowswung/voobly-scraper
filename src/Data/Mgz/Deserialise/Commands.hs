{-# OPTIONS -fno-warn-deprecations #-}

module Data.Mgz.Deserialise.Commands where

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import Data.Binary.Get (Get)
-- Partial deserialiser

import Data.Mgz.Deserialise.BasicTypes


data Op =
    OpTypeSync OpSync
  | OpTypeCommand Command
  | OpTypeMetaGameStart
  | OpTypeMetaChat Text
  | OpTypeUnhandled Int
 deriving (Show)


data OpSync = OpSync {
  opSyncTime :: Int
} deriving (Show, Eq, Ord)

opCommand :: Op -> Maybe Command
opCommand (OpTypeCommand c) = Just c
opCommand _ = Nothing


instance SimpleGet Op where
  simpleGet = do
    opType <- parseInt32
    case opType of
      1 -> OpTypeCommand <$> simpleGet
      2 -> do
         t <- parseInt32
         u <- parseInt32
         when (u == 0) $ G.skip 28
         G.skip 12
         pure $ OpTypeSync (OpSync t)
      4 -> do
        command <- parseInt32
        case command of
          -1 -> do
            l <- parseInt32
            c <- takeText l
            pure $ OpTypeMetaChat c
          500 -> do
            G.skip 20
            pure OpTypeMetaGameStart
          _ -> fail $ "unhandled meta command: " ++ show command
      n | n > 1000 -> pure $ OpTypeUnhandled opType
        | otherwise -> fail $  "unhandled opType: " ++ show opType






data Command =
    CommandTypePrimary CommandPrimary
  | CommandTypeMove CommandMove
  | CommandTypeStance CommandStance
  | CommandTypeGuard CommandGuard
  | CommandTypeFollow CommandFollow
  | CommandTypePatrol CommandPatrol
  | CommandTypeFormation CommandFormation
  | CommandTypeResearch CommandResearch
  | CommandTypeBuild CommandBuild
  | CommandTypeWall CommandWall
  | CommandTypeTrain CommandTrain
  | CommandTypeWaypoint CommandWaypoint
  | CommandTypeStop CommandStop
  | CommandTypeRally CommandRally
  | CommandTypeDelete CommandDelete
  | CommandUnparsed Int ByteString
    deriving (Show, Eq, Ord)


instance SimpleGet Command where
  simpleGet = do
    l <- parseInt32
    G.isolate l $ do
      cId <- parseInt8
      case cId of
        0 -> CommandTypePrimary <$> simpleGet
        1 -> CommandTypeStop <$> simpleGet
        3 -> CommandTypeMove <$> simpleGet
        10 -> CommandTypeFollow <$> simpleGet
        16 -> CommandTypeWaypoint <$> simpleGet
        18 -> CommandTypeStance <$> simpleGet
        19 -> CommandTypeGuard <$> simpleGet
        21 -> CommandTypePatrol <$> simpleGet
        22 -> CommandTypeFormation <$> simpleGet
        101 -> CommandTypeResearch <$> simpleGet
        102 -> CommandTypeBuild <$> simpleGet
        105 -> CommandTypeWall <$> simpleGet
        106 -> CommandTypeDelete <$> simpleGet
        119 -> CommandTypeTrain <$> simpleGet
        120 -> CommandTypeRally <$> simpleGet
        n -> CommandUnparsed n <$> (fmap BL.toStrict $ G.getRemainingLazyByteString)



data CommandPrimary = CommandPrimary {
  commandPrimaryPlayerId :: Int
, commandPrimaryTargetId :: Maybe Int
, commandPrimaryPos :: Pos
, commandPrimaryUnitIds :: EitherInheritOrIds
} deriving (Show, Eq, Ord)

instance SimpleGet CommandPrimary where
  simpleGet = do
    commandPrimaryPlayerId <- parseInt8
    G.skip 2
    commandPrimaryTargetId <- fmap (\t -> if t < 1 then Nothing else Just t) parseInt32
    selectCount <- parseInt8
    G.skip 3
    commandPrimaryPos <- getPos
    commandPrimaryUnitIds <- getSelectedUnitsOrInherit selectCount
    pure CommandPrimary{..}

data CommandMove = CommandMove {
  commandMovePlayerId :: Int
, commandMovePos :: Pos
, commandMoveUnitIds :: EitherInheritOrIds
} deriving (Show, Eq, Ord)

instance SimpleGet CommandMove where
  simpleGet = do
    commandMovePlayerId <- parseInt8
    G.skip 6
    selectCount <- parseInt32
    commandMovePos <- getPos
    commandMoveUnitIds <- getSelectedUnitsOrInherit selectCount
    pure CommandMove{..}


data CommandStance = CommandStance {
  commandStanceStance :: Int
, commandStanceUnitIds :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandStance where
  simpleGet = do
    selectCount <- parseInt8
    commandStanceStance <- parseInt8
    commandStanceUnitIds <- getSelectedUnits selectCount
    pure CommandStance{..}


data CommandGuard = CommandGuard {
  commandGuardGuarded :: Int
, commandGuardUnitIds :: [Int]
} deriving (Show, Eq, Ord)




instance SimpleGet CommandGuard where
  simpleGet = do
    selectCount <- parseInt8
    G.skip 2
    commandGuardGuarded <- parseInt32
    commandGuardUnitIds <- getSelectedUnits selectCount
    pure CommandGuard{..}

data CommandFollow = CommandFollow {
  commandFollowFollowed :: Int
, commandFollowUnitIds :: [Int]
} deriving (Show, Eq, Ord)



instance SimpleGet CommandFollow where
  simpleGet = do
    selectCount <- parseInt8
    G.skip 2
    commandFollowFollowed <- parseInt32
    commandFollowUnitIds <- getSelectedUnits selectCount
    pure CommandFollow{..}

data CommandPatrol = CommandPatrol {
  commandPatrolWaypoints :: [Pos]
, commandPatrolUnitIds :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandPatrol where
  simpleGet = do
    selectCount <- parseInt8
    waypointCount <- parseInt8
    G.skip 1
    commandPatrolWaypoints <- getMultiplePos waypointCount
    commandPatrolUnitIds <- getSelectedUnits selectCount
    pure CommandPatrol{..}

data CommandFormation = CommandFormation {
  commandFormationPlayerId :: Int
, commandFormationFormation :: Int
, commandFormationUnitIds :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandFormation where
  simpleGet = do
    selectCount <- parseInt8
    commandFormationPlayerId <- parseInt32
    G.skip 1
    commandFormationFormation <- parseInt8
    G.skip 3
    commandFormationUnitIds <- getSelectedUnits selectCount
    pure CommandFormation{..}



data CommandResearch = CommandResearch {
  commandResearchBuildingId :: Int
, commandResearchPlayerId :: Int
, commandResearchResearch :: Int
} deriving (Show, Eq, Ord)


instance SimpleGet CommandResearch where
  simpleGet = do
    G.skip 3
    commandResearchBuildingId <- parseInt32
    commandResearchPlayerId <- parseInt8
    G.skip 1
    commandResearchResearch <- parseInt16
    G.skip 4
    pure CommandResearch{..}

data CommandBuild = CommandBuild {
  commandBuildPlayerId :: Int
, commandBuildPos :: Pos
, commandBuildBuildingType :: Int
, commandBuildBuilders :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandBuild where
  simpleGet = do
    selectCount <- parseInt8
    commandBuildPlayerId <- parseInt8
    G.skip 1
    commandBuildPos <- getPos
    commandBuildBuildingType <- parseInt16
    G.skip 10
    commandBuildBuilders <- getSelectedUnits selectCount
    pure CommandBuild{..}



instance SimpleGet CommandWall where
  simpleGet = do
    selectCount <- parseInt8
    commandWallPlayerId <- parseInt8
    commandWallStartPos <- PosSimple <$> parseInt8 <*> parseInt8
    commandWallEndPos <- PosSimple <$> parseInt8 <*> parseInt8
    G.skip 1
    commandWallBuildingType <- parseInt16
    G.skip 6
    commandWallBuilders <- getSelectedUnits selectCount
    pure CommandWall{..}

data CommandWall = CommandWall {
  commandWallPlayerId :: Int
, commandWallStartPos :: PosSimple
, commandWallEndPos :: PosSimple
, commandWallBuildingType :: Int
, commandWallBuilders :: [Int]
} deriving (Show, Eq, Ord)

data CommandTrain = CommandTrain {
  commandTrainBuildingId :: Int
, commandTrainUnitType :: Int
, commandTrainNumber :: Int
} deriving (Show, Eq, Ord)




instance SimpleGet CommandTrain where
  simpleGet = do
    G.skip 3
    commandTrainBuildingId <- parseInt32
    commandTrainUnitType <- parseInt16
    commandTrainNumber <- parseInt16
    pure CommandTrain{..}


data CommandWaypoint = CommandWaypoint {
  commandWaypointPlayerId :: Int
, commandWaypointSelectedIds ::  EitherInheritOrIds -- can be building ids?
, commandWaypointPos :: PosSimple
} deriving (Show, Eq, Ord)



instance SimpleGet CommandWaypoint where
  simpleGet = do
    commandWaypointPlayerId <- parseInt8
    selectCount <- parseInt8
    commandWaypointPos <- PosSimple <$> parseInt8 <*> parseInt8
    commandWaypointSelectedIds <- getSelectedUnitsOrInherit selectCount
    pure CommandWaypoint{..}


data CommandStop = CommandStop {
  commandStopSelectedIds :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandStop where
  simpleGet = do
    selectCount <- parseInt8
    commandStopSelectedIds <- getSelectedUnits selectCount
    pure CommandStop{..}


data CommandRally = CommandRally {
  commandRallyTargetObject :: Maybe Int
, commandRallyTargetType :: Maybe Int
, commandRallyPos :: Pos
, commandRallySelectedBuildingIds :: [Int]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandRally where
  simpleGet = do
    selectCount <- parseInt8
    G.skip 2
    mto <- parseInt32
    mtt <- parseInt32
    let commandRallyTargetObject = if mto == -1 then Nothing else Just mto
        commandRallyTargetType = if mtt == 65535 then Nothing else Just mtt
    commandRallyPos <- getPos
    commandRallySelectedBuildingIds <- getSelectedUnits selectCount
    pure CommandRally{..}


data CommandDelete = CommandDelete {
  commandDeleteObjectId :: Int
, commandDeletePlayerId :: Int
} deriving (Show, Eq, Ord)


instance SimpleGet CommandDelete where
  simpleGet = do
    G.skip 3
    commandDeleteObjectId <- parseInt32
    commandDeletePlayerId <- parseInt8
    G.skip 3
    pure CommandDelete{..}


{-
rendering etc
-}



commandToTypeText :: Command -> Text
commandToTypeText (CommandTypePrimary _) = "Primary"
commandToTypeText (CommandTypeMove _) = "Move"
commandToTypeText (CommandTypeStance _) = "Stance"
commandToTypeText (CommandTypeGuard _) = "Guard"
commandToTypeText (CommandTypeFollow _) = "Follow"
commandToTypeText (CommandTypePatrol _) = "Patrol"
commandToTypeText (CommandTypeFormation _) = "Formation"
commandToTypeText (CommandTypeResearch _) = "Research"
commandToTypeText (CommandTypeBuild _) = "Build"
commandToTypeText (CommandTypeWall _) = "Wall"
commandToTypeText (CommandTypeTrain _) = "Train"
commandToTypeText (CommandTypeWaypoint _) = "Waypoint"
commandToTypeText (CommandTypeStop _) = "Stop"
commandToTypeText (CommandTypeRally _) = "Rally"
commandToTypeText (CommandTypeDelete _) = "Delete"
commandToTypeText (CommandUnparsed n _) = "Unparsed: " <> displayShowT n


debugCommand :: Op -> Get ()
debugCommand (OpTypeCommand c) =
  case c of
    CommandTypePrimary CommandPrimary{..} -> do
      traceM $ "Player " <> displayShowT commandPrimaryPlayerId <> " primaried " <> displayShowT commandPrimaryTargetId <> " with " <> displayShowT (length commandPrimaryUnitIds) <> " units"

    CommandTypeMove CommandMove{..} -> do
      traceM $ "Player " <> displayShowT commandMovePlayerId <> " moved to  " <> displayShowT (posX commandMovePos, posY commandMovePos) <> " with " <> displayShowT (length commandMoveUnitIds) <> " units"
    CommandUnparsed _ _-> pure ()
    _ -> traceM $ displayShowT $ commandToTypeText c

debugCommand (OpTypeSync (OpSync t)) = traceM $ "SYNC: " <> displayShowT t
debugCommand _ = pure ()

