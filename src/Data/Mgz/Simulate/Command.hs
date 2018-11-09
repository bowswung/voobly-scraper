module Data.Mgz.Simulate.Command where
import RIO

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.State
import Data.Mgz.Simulate.Events



handleCommand :: Command -> Sim ()
handleCommand c = do
  mEt <- runCommand c
  case mEt of
    Nothing -> pure ()
    Just et -> addRealEvent c (commandPlayerId c) et

class RunCommand a where
  runCommand :: a -> Sim (Maybe EventType)

instance RunCommand Command where
  runCommand (CommandTypePrimary c) = runCommand c
  runCommand (CommandTypeMove c) = runCommand c
  runCommand (CommandTypeStance c) = runCommand c
  runCommand (CommandTypeGuard c) = runCommand c
  runCommand (CommandTypeFollow c) = runCommand c
  runCommand (CommandTypePatrol c) = runCommand c
  runCommand (CommandTypeFormation c) = runCommand c
  runCommand (CommandTypeResearch c) = runCommand c
  runCommand (CommandTypeBuild c) = runCommand c
  runCommand (CommandTypeTrain c) = runCommand c
  runCommand (CommandTypeWaypoint c) = runCommand c
  runCommand (CommandTypeStop c) = runCommand c
  runCommand (CommandTypeRally c) = runCommand c
  runCommand (CommandTypeDelete c) = runCommand c
  runCommand (CommandUnparsed _ _) = pure Nothing
  runCommand (CommandTypeWall _) = pure Nothing
  runCommand _  = pure Nothing

commandPlayerId :: Command -> Maybe PlayerId
commandPlayerId (CommandTypePrimary CommandPrimary{..}) = Just commandPrimaryPlayerId
commandPlayerId (CommandTypeMove CommandMove{..}) = Just commandMovePlayerId
commandPlayerId (CommandTypeFormation CommandFormation{..}) = Just commandFormationPlayerId
commandPlayerId (CommandTypeResearch CommandResearch{..}) = Just commandResearchPlayerId
commandPlayerId (CommandTypeBuild CommandBuild{..}) = Just commandBuildPlayerId
commandPlayerId (CommandTypeWall CommandWall{..}) = Just commandWallPlayerId
commandPlayerId (CommandTypeWaypoint CommandWaypoint{..}) = Just commandWaypointPlayerId
commandPlayerId (CommandTypeDelete CommandDelete{..}) = Just commandDeletePlayerId
commandPlayerId _ = Nothing

instance RunCommand CommandPrimary where
  runCommand CommandPrimary{..} = do
    case commandPrimaryTargetId of
      Just tid -> do
        target <- getObject tid
        uids <- getSelectedObjectIds commandPrimaryUnitIds commandPrimaryPlayerId
        objs <- getObjectsForPlayer uids commandPrimaryPlayerId
        pure . Just . EventTypePrimary $ EventPrimary {
            eventPrimaryObjects = map objectId objs
          , eventPrimaryTarget = objectId target
          , eventPrimaryPos = commandPrimaryPos
          }
      Nothing -> do
        uids <- getSelectedObjectIds commandPrimaryUnitIds commandPrimaryPlayerId
        units <- getUnitsForPlayer uids commandPrimaryPlayerId
        pure . Just. EventTypeMove $ EventMove {
            eventMoveUnits = map unitId units
            , eventMovePos = commandPrimaryPos
          }

instance RunCommand CommandMove where
  runCommand CommandMove{..} = do
    uids <- getSelectedObjectIds commandMoveUnitIds commandMovePlayerId
    units <- getUnitsForPlayer uids commandMovePlayerId
    pure . Just $ EventTypeMove $ EventMove {
        eventMoveUnits = map unitId units
      , eventMovePos = commandMovePos
      }

instance RunCommand CommandStance where
  runCommand CommandStance{..} = do
    units <- mapM getUnit commandStanceUnitIds
    pure . Just $ EventTypeMilitaryDisposition $ EventMilitaryDisposition {
        eventMilitaryDispositionUnits = map unitId units
      , eventMilitaryDispositionType = MilitaryDispositionStance commandStanceStance
      }
instance RunCommand CommandFormation where
  runCommand CommandFormation{..} = do
    units <- getUnitsForPlayer commandFormationUnitIds commandFormationPlayerId
    pure . Just $  EventTypeMilitaryDisposition $ EventMilitaryDisposition {
        eventMilitaryDispositionUnits = map unitId units
      , eventMilitaryDispositionType = MilitaryDispositionFormation commandFormationFormation
      }
instance RunCommand CommandGuard where
  runCommand CommandGuard{..} = do
    target <- getObject commandGuardGuarded
    units <- mapM getUnit commandGuardUnitIds

    pure . Just $ EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
        eventTargetedMilitaryOrderUnits = map unitId units
      , eventTargetedMilitaryOrderType = TargetedMilitaryOrderGuard
      , eventTargetedMilitaryOrderTarget = objectId target
      }
instance RunCommand CommandFollow where
  runCommand CommandFollow{..} = do
    target <- getObject commandFollowFollowed
    units <- mapM getUnit commandFollowUnitIds

    pure . Just $ EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
        eventTargetedMilitaryOrderUnits = map unitId units
      , eventTargetedMilitaryOrderType = TargetedMilitaryOrderFollow
      , eventTargetedMilitaryOrderTarget = objectId target
      }
instance RunCommand CommandPatrol where
  runCommand CommandPatrol{..} = do
    units <- mapM getUnit commandPatrolUnitIds

    pure . Just $ EventTypePatrol $ EventPatrol {
        eventPatrolUnits = map unitId units
      , eventPatrolWaypoints = commandPatrolWaypoints
      }
instance RunCommand CommandBuild where
  runCommand CommandBuild{..} = do
    units <- getUnitsForPlayer commandBuildBuilders commandBuildPlayerId

    pure . Just $ EventTypeBuild $ EventBuild {
        eventBuildBuilders = map unitId units
      , eventBuildPos = commandBuildPos
      , eventBuildingType = BuildingTypeKnown commandBuildBuildingType
      , eventBuildBuilding = Nothing
      }
instance RunCommand CommandResearch where
  runCommand CommandResearch{..} = do
    building <- getBuildingForPlayer commandResearchBuildingId commandResearchPlayerId

    pure . Just $ EventTypeResearch $ EventResearch {
        eventResearchBuilding = buildingId building
      , eventResearchTech = normalizeTech commandResearchResearch
      }
instance RunCommand CommandTrain where
  runCommand CommandTrain{..} = do
    building <- getBuilding commandTrainBuildingId
    pure . Just $ EventTypeTrain $ EventTrain {
        eventTrainBuilding = buildingId building
      , eventTrainType = objectTypeToUnitType $ commandTrainUnitType
      , eventTrainNumber = commandTrainNumber
      }
instance RunCommand CommandStop where
  runCommand CommandStop{..} = do
    objs <- mapM getObject commandStopSelectedIds

    pure . Just $ EventTypeStopGeneral $ EventStopGeneral {
        eventStopSelectedIds = map objectId objs
      }
instance RunCommand CommandWaypoint where
  runCommand CommandWaypoint{..} = do
    uids <- getSelectedObjectIds commandWaypointSelectedIds commandWaypointPlayerId
    objs <- getObjectsForPlayer uids commandWaypointPlayerId
    pure . Just $ EventTypeWaypoint $ EventWaypoint {
        eventWaypointSelectedObjects = map objectId objs,
        eventWaypointPos = commandWaypointPos
      }
instance RunCommand CommandRally where
  runCommand CommandRally{..} = do
    targetObj <-
      case (commandRallyTargetObject, commandRallyTargetType) of
        (Nothing, Nothing) -> pure Nothing
        (Just o, Just t) -> fmap Just $ getObjectAsType o t
        (a, b) -> error $ "Rally command with inconsistent targets " ++ show (a,b)

    buildings <- mapM getBuilding commandRallySelectedBuildingIds

    pure . Just $ EventTypeRally $ EventRally {
        eventRallyTargetObject = fmap objectId targetObj,
        eventRallyPos = commandRallyPos,
        eventRallyBuildings = map buildingId buildings
      }
instance RunCommand CommandDelete where
  runCommand CommandDelete{..} = do
    target <- getObjectForPlayer commandDeleteObjectId (Just commandDeletePlayerId)

    pure . Just $ EventTypeDelete $ EventDelete {
        eventDeleteObjectId = objectId target
      }





