{-# OPTIONS -fno-warn-deprecations #-}

module Data.Mgz.Deserialise.Commands where

import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import Data.Binary.Get (Get)
-- Partial deserialiser

import Data.Mgz.Deserialise.BasicTypes
import Data.Mgz.Constants


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
    opType <- getInt32Int
    case opType of
      1 -> OpTypeCommand <$> simpleGet
      2 -> do
         t <- getInt32Int
         u <- getInt32Int
         when (u == 0) $ G.skip 28
         G.skip 12
         pure $ OpTypeSync (OpSync t)
      4 -> do
        command <- getInt32Int
        case command of
          -1 -> do
            l <- getInt32Int
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
  | CommandTypeResign CommandResign
  | CommandTypeAttackGround CommandAttackGround
  | CommandTypeTribute CommandTribute
  | CommandTypeRepair CommandRepair
  | CommandTypeUngarrison CommandUngarrison
  | CommandTypeToggleGate CommandToggleGate
  | CommandTypeGarrison CommandGarrison
  | CommandTypeSell CommandSell
  | CommandTypeBuy CommandBuy
  | CommandTypeDropRelic CommandDropRelic
  | CommandTypeTownBell CommandTownBell
  | CommandTypeBackToWork CommandBackToWork
  | CommandUnparsed Int ByteString
    deriving (Show, Eq, Ord)


instance SimpleGet Command where
  simpleGet = do
    l <- getInt32Int
    G.isolate l $ do
      cId <- getInt8Int
      case cId of
        0 -> CommandTypePrimary <$> simpleGet
        1 -> CommandTypeStop <$> simpleGet
        3 -> CommandTypeMove <$> simpleGet
        16 -> CommandTypeWaypoint <$> simpleGet
        18 -> CommandTypeStance <$> simpleGet
        19 -> CommandTypeGuard <$> simpleGet
        20 -> CommandTypeFollow <$> simpleGet
        21 -> CommandTypePatrol <$> simpleGet
        22 -> CommandTypeFormation <$> simpleGet
        101 -> CommandTypeResearch <$> simpleGet
        102 -> CommandTypeBuild <$> simpleGet
        105 -> CommandTypeWall <$> simpleGet
        106 -> CommandTypeDelete <$> simpleGet
        119 -> CommandTypeTrain <$> simpleGet
        120 -> CommandTypeRally <$> simpleGet
        11 -> CommandTypeResign <$> simpleGet
        107 -> CommandTypeAttackGround <$> simpleGet
        108 -> CommandTypeTribute <$> simpleGet
        110 -> CommandTypeRepair <$> simpleGet
        111 -> CommandTypeUngarrison <$> simpleGet
        114 -> CommandTypeToggleGate <$> simpleGet
        117 -> CommandTypeGarrison <$> simpleGet
        122 -> CommandTypeSell <$> simpleGet
        123 -> CommandTypeBuy <$> simpleGet
        126 -> CommandTypeDropRelic <$> simpleGet
        127 -> CommandTypeTownBell <$> simpleGet
        128 -> CommandTypeBackToWork <$> simpleGet
        n -> CommandUnparsed n <$> (fmap BL.toStrict $ G.getRemainingLazyByteString)



data CommandPrimary = CommandPrimary {
  commandPrimaryPlayerId :: PlayerId
, commandPrimaryTargetId :: Maybe ObjectId
, commandPrimaryPos :: Pos
, commandPrimaryUnitIds :: EitherInheritOrIds
} deriving (Show, Eq, Ord)

instance SimpleGet CommandPrimary where
  simpleGet = do
    commandPrimaryPlayerId <- fmap PlayerId getInt8Int
    G.skip 2
    commandPrimaryTargetId <- getMaybeObjectId
    selectCount <- getInt8Int
    G.skip 3
    commandPrimaryPos <- getPos
    commandPrimaryUnitIds <- getSelectedUnitsOrInherit selectCount
    pure CommandPrimary{..}

data CommandMove = CommandMove {
  commandMovePlayerId :: PlayerId
, commandMovePos :: Pos
, commandMoveUnitIds :: EitherInheritOrIds
} deriving (Show, Eq, Ord)

instance SimpleGet CommandMove where
  simpleGet = do
    commandMovePlayerId <- fmap PlayerId getInt8Int
    G.skip 6
    selectCount <- getInt32Int
    commandMovePos <- getPos
    commandMoveUnitIds <- getSelectedUnitsOrInherit selectCount
    pure CommandMove{..}


data CommandStance = CommandStance {
  commandStanceStance :: Int
, commandStanceUnitIds :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandStance where
  simpleGet = do
    selectCount <- getInt8Int
    commandStanceStance <- getInt8Int
    commandStanceUnitIds <- getSelectedUnits selectCount
    pure CommandStance{..}


data CommandGuard = CommandGuard {
  commandGuardGuarded :: ObjectId
, commandGuardUnitIds :: [ObjectId]
} deriving (Show, Eq, Ord)




instance SimpleGet CommandGuard where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandGuardGuarded <- fmap ObjectId getInt32Int
    commandGuardUnitIds <- getSelectedUnits selectCount
    pure CommandGuard{..}

data CommandFollow = CommandFollow {
  commandFollowFollowed :: ObjectId
, commandFollowUnitIds :: [ObjectId]
} deriving (Show, Eq, Ord)



instance SimpleGet CommandFollow where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandFollowFollowed <-  fmap ObjectId getInt32Int
    commandFollowUnitIds <- getSelectedUnits selectCount
    pure CommandFollow{..}

data CommandPatrol = CommandPatrol {
  commandPatrolWaypoints :: [Pos]
, commandPatrolUnitIds :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandPatrol where
  simpleGet = do
    selectCount <- getInt8Int
    waypointCount <- getInt8Int
    G.skip 1
    commandPatrolWaypoints <- getMultiplePos waypointCount
    commandPatrolUnitIds <- getSelectedUnits selectCount
    pure CommandPatrol{..}

data CommandFormation = CommandFormation {
  commandFormationPlayerId :: PlayerId
, commandFormationFormation :: Int
, commandFormationUnitIds :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandFormation where
  simpleGet = do
    selectCount <- getInt8Int
    commandFormationPlayerId <- fmap PlayerId getInt32Int
    G.skip 1
    commandFormationFormation <- getInt8Int
    G.skip 3
    commandFormationUnitIds <- getSelectedUnits selectCount
    pure CommandFormation{..}



data CommandResearch = CommandResearch {
  commandResearchBuildingId :: ObjectId
, commandResearchPlayerId :: PlayerId
, commandResearchResearch :: Int
} deriving (Show, Eq, Ord)


instance SimpleGet CommandResearch where
  simpleGet = do
    G.skip 3
    commandResearchBuildingId <- getObjectId
    commandResearchPlayerId <- fmap PlayerId getInt8Int
    G.skip 1
    commandResearchResearch <- getInt16Int
    G.skip 4
    pure CommandResearch{..}

data CommandBuild = CommandBuild {
  commandBuildPlayerId :: PlayerId
, commandBuildPos :: Pos
, commandBuildBuildingType :: ObjectType
, commandBuildBuilders :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandBuild where
  simpleGet = do
    selectCount <- getInt8Int
    commandBuildPlayerId <- fmap PlayerId getInt8Int
    G.skip 1
    commandBuildPos <- getPos
    commandBuildBuildingType <- fmap normaliseObjectType getInt16Int
    G.skip 10
    commandBuildBuilders <- getSelectedUnits selectCount
    pure CommandBuild{..}


data CommandWall = CommandWall {
  commandWallPlayerId :: PlayerId
, commandWallStartPos :: PosSimple
, commandWallEndPos :: PosSimple
, commandWallBuildingType :: ObjectType
, commandWallBuilders :: [ObjectId]
} deriving (Show, Eq, Ord)

instance SimpleGet CommandWall where
  simpleGet = do
    selectCount <- getInt8Int
    commandWallPlayerId <- fmap PlayerId getInt8Int
    commandWallStartPos <- PosSimple <$> getInt8Int <*> getInt8Int
    commandWallEndPos <- PosSimple <$> getInt8Int <*> getInt8Int
    G.skip 1
    commandWallBuildingType <- fmap normaliseObjectType getInt16Int
    G.skip 6
    commandWallBuilders <- getSelectedUnits selectCount
    pure CommandWall{..}


data CommandTrain = CommandTrain {
  commandTrainBuildingId :: ObjectId
, commandTrainUnitType :: ObjectType
, commandTrainNumber :: Int
} deriving (Show, Eq, Ord)




instance SimpleGet CommandTrain where
  simpleGet = do
    G.skip 3
    commandTrainBuildingId <- getObjectId
    commandTrainUnitType <- fmap normaliseObjectType getInt16Int
    commandTrainNumber <- getInt16Int
    pure CommandTrain{..}


data CommandWaypoint = CommandWaypoint {
  commandWaypointPlayerId :: PlayerId
, commandWaypointSelectedIds ::  EitherInheritOrIds -- can be building ids?
, commandWaypointPos :: PosSimple
} deriving (Show, Eq, Ord)



instance SimpleGet CommandWaypoint where
  simpleGet = do
    commandWaypointPlayerId <- fmap PlayerId getInt8Int
    selectCount <- getInt8Int
    commandWaypointPos <- PosSimple <$> getInt8Int <*> getInt8Int
    commandWaypointSelectedIds <- getSelectedUnitsOrInherit selectCount
    pure CommandWaypoint{..}


data CommandStop = CommandStop {
  commandStopSelectedIds :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandStop where
  simpleGet = do
    selectCount <- getInt8Int
    commandStopSelectedIds <- getSelectedUnits selectCount
    pure CommandStop{..}


data CommandRally = CommandRally {
  commandRallyTargetObject :: Maybe ObjectId
, commandRallyTargetType :: Maybe ObjectType
, commandRallyPos :: Pos
, commandRallySelectedBuildingIds :: [ObjectId]
} deriving (Show, Eq, Ord)


instance SimpleGet CommandRally where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandRallyTargetObject <- getMaybeObjectId
    commandRallyTargetType <- fmap normaliseObjectTypeMaybe getInt32Int
    commandRallyPos <- getPos
    commandRallySelectedBuildingIds <- getSelectedUnits selectCount
    pure CommandRally{..}


data CommandDelete = CommandDelete {
  commandDeleteObjectId :: ObjectId
, commandDeletePlayerId :: PlayerId
} deriving (Show, Eq, Ord)


instance SimpleGet CommandDelete where
  simpleGet = do
    G.skip 3
    commandDeleteObjectId <- getObjectId
    commandDeletePlayerId <- fmap PlayerId getInt8Int
    G.skip 3
    pure CommandDelete{..}


data CommandResign = CommandResign {
  commandResignPlayerId :: PlayerId
} deriving (Show, Eq, Ord)

instance SimpleGet CommandResign where
  simpleGet = do
    G.skip 1
    commandResignPlayerId <- fmap PlayerId getInt8Int
    G.skip 1
    pure CommandResign{..}

data CommandAttackGround = CommandAttackGround {
  commandAttackGroundSelectedIds :: [ObjectId],
  commandAttackGroundPos :: Pos
} deriving (Show, Eq, Ord)

instance SimpleGet CommandAttackGround where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandAttackGroundPos <- getPos
    commandAttackGroundSelectedIds <- getSelectedUnits selectCount
    pure CommandAttackGround{..}


data CommandTribute = CommandTribute {
  commandTributeFrom :: PlayerId,
  commandTributeTo :: PlayerId,
  commandTributeResourceKind :: ResourceKind,
  commandTributeAmount :: Float,
  commanndTributeTransationFee :: Float
} deriving (Show, Eq, Ord)

instance SimpleGet CommandTribute where
  simpleGet = do
    commandTributeFrom <- fmap PlayerId getInt8Int
    commandTributeTo <- fmap PlayerId getInt8Int
    commandTributeResourceKind <- getResourceKind
    commandTributeAmount <- G.getFloatle
    commanndTributeTransationFee <- G.getFloatle
    pure CommandTribute{..}

data CommandRepair = CommandRepair {
  commandRepairRepaired :: ObjectId,
  commandRepairRepairers :: [ObjectId]
} deriving (Show, Eq, Ord)

instance SimpleGet CommandRepair where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandRepairRepaired <- getObjectId
    commandRepairRepairers <- getSelectedUnits selectCount
    pure CommandRepair{..}

data CommandUngarrison = CommandUngarrison {
  commandUngarrisonPos :: Maybe Pos, -- if from building then doesn't exist
  commandUngarrisonType :: Int,
  commandUngarrisonObjectClicked :: Maybe ObjectId,
  commandUngarrisonReleasedFrom :: [ObjectId]
} deriving (Show, Eq, Ord)

instance SimpleGet CommandUngarrison where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandUngarrisonPos <- getMaybePos
    commandUngarrisonType <- getInt8Int
    G.skip 3
    commandUngarrisonObjectClicked <- getMaybeObjectId
    commandUngarrisonReleasedFrom <- getSelectedUnits selectCount
    pure CommandUngarrison{..}

data CommandToggleGate = CommandToggleGate {
  commandToggleGateGate:: ObjectId
} deriving (Show, Eq, Ord)

instance SimpleGet CommandToggleGate where
  simpleGet = do
    G.skip 3
    commandToggleGateGate <- getObjectId
    pure CommandToggleGate{..}

data CommandGarrison = CommandGarrison {
  commandGarrisonTargetId:: Maybe ObjectId,
  commandGarrisonType :: GarrisonType,
  commandGarrisonPos :: Pos, -- this might be misleading, the pos doesn't seem to be used in the normal way
  commandGarrisonSelectedIds :: [ObjectId]

} deriving (Show, Eq, Ord)

instance SimpleGet CommandGarrison where
  simpleGet = do
    selectCount <- getInt8Int
    G.skip 2
    commandGarrisonTargetId <- getMaybeObjectId
    commandGarrisonType <- getGarrisonType
    G.skip 3
    commandGarrisonPos <- getPos
    G.skip 4
    commandGarrisonSelectedIds <- getSelectedUnits selectCount
    pure CommandGarrison{..}

data CommandSell = CommandSell {
  commandSellPlayer :: PlayerId,
  commandSellKind :: ResourceKind,
  commandSellAmount :: Int, -- in hundreds
  commandSellMarket :: ObjectId

} deriving (Show, Eq, Ord)

instance SimpleGet CommandSell where
  simpleGet = do
    commandSellPlayer <- fmap PlayerId getInt8Int
    commandSellKind <- getResourceKind
    commandSellAmount <- getInt8Int
    commandSellMarket <- getObjectId
    pure CommandSell{..}

data CommandBuy = CommandBuy {
  commandBuyPlayer :: PlayerId,
  commandBuyKind :: ResourceKind,
  commandBuyAmount :: Int, -- in hundreds
  commandBuyMarket :: ObjectId

} deriving (Show, Eq, Ord)

instance SimpleGet CommandBuy where
  simpleGet = do
    commandBuyPlayer <- fmap PlayerId getInt8Int
    commandBuyKind <- getResourceKind
    commandBuyAmount <- getInt8Int
    commandBuyMarket <- getObjectId
    pure CommandBuy{..}

data CommandDropRelic = CommandDropRelic {
  commandDropRelicMonkId :: ObjectId

} deriving (Show, Eq, Ord)

instance SimpleGet CommandDropRelic where
  simpleGet = do
    G.skip 3
    commandDropRelicMonkId <- getObjectId
    pure CommandDropRelic{..}

data CommandTownBell = CommandTownBell {
  commandTownBellTownCenter :: ObjectId,
  commandTownBellActive :: Bool
} deriving (Show, Eq, Ord)

instance SimpleGet CommandTownBell where
  simpleGet = do
    G.skip 3
    commandTownBellTownCenter <- getObjectId
    commandTownBellActive <- getBool
    G.skip 3
    pure CommandTownBell{..}

data CommandBackToWork = CommandBackToWork {
  commandBackToWorkBuildingId :: ObjectId
} deriving (Show, Eq, Ord)

instance SimpleGet CommandBackToWork where
  simpleGet = do
    G.skip 3
    commandBackToWorkBuildingId <- getObjectId
    pure CommandBackToWork{..}

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
commandToTypeText (CommandTypeResign _) = "Resign"
commandToTypeText (CommandTypeAttackGround _) = "AttackGround"
commandToTypeText (CommandTypeTribute _) = "Tribute"
commandToTypeText (CommandTypeRepair _) = "Repair"
commandToTypeText (CommandTypeUngarrison _) = "Ungarrison"
commandToTypeText (CommandTypeToggleGate _) = "ToggleGate"
commandToTypeText (CommandTypeGarrison _) = "Garrison"
commandToTypeText (CommandTypeSell _) = "Sell"
commandToTypeText (CommandTypeBuy _) = "Buy"
commandToTypeText (CommandTypeDropRelic _) = "DropRelic"
commandToTypeText (CommandTypeTownBell _) = "TownBell"
commandToTypeText (CommandTypeBackToWork _) = "BackToWork"
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

