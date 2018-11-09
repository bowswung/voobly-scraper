module Data.Mgz.Simulate.Command where
import RIO

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.State
import Data.Mgz.Simulate.Events

addCommandAsEvent :: Command -> Sim ()
addCommandAsEvent c@(CommandTypePrimary CommandPrimary{..}) = do
  eType <-
    case commandPrimaryTargetId of
      Just tid -> do
        target <- getObject tid
        uids <- getSelectedObjectIds commandPrimaryUnitIds commandPrimaryPlayerId
        objs <- getObjectsForPlayer uids commandPrimaryPlayerId
        pure $ EventTypePrimary $ EventPrimary {
            eventPrimaryObjects = map objectId objs
          , eventPrimaryTarget = objectId target
          , eventPrimaryPos = commandPrimaryPos
          }
      Nothing -> do
          uids <- getSelectedObjectIds commandPrimaryUnitIds commandPrimaryPlayerId
          units <- getUnitsForPlayer uids commandPrimaryPlayerId
          pure . EventTypeMove $ EventMove {
              eventMoveUnits = map unitId units
            , eventMovePos = commandPrimaryPos
            }
  addRealEvent c (Just commandPrimaryPlayerId) eType

addCommandAsEvent c@(CommandTypeMove CommandMove{..}) = do
  uids <- getSelectedObjectIds commandMoveUnitIds commandMovePlayerId
  units <- getUnitsForPlayer uids commandMovePlayerId
  let eType = EventTypeMove $ EventMove {
      eventMoveUnits = map unitId units
    , eventMovePos = commandMovePos
    }
  addRealEvent c (Just commandMovePlayerId) eType

addCommandAsEvent c@(CommandTypeStance CommandStance{..}) = do
  units <- mapM getUnit commandStanceUnitIds
  let eType = EventTypeMilitaryDisposition $ EventMilitaryDisposition {
      eventMilitaryDispositionUnits = map unitId units
    , eventMilitaryDispositionType = MilitaryDispositionStance commandStanceStance
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeFormation CommandFormation{..}) = do
  units <- getUnitsForPlayer commandFormationUnitIds commandFormationPlayerId

  let eType = EventTypeMilitaryDisposition $ EventMilitaryDisposition {
      eventMilitaryDispositionUnits = map unitId units
    , eventMilitaryDispositionType = MilitaryDispositionFormation commandFormationFormation
    }
  addRealEvent c (Just commandFormationPlayerId) eType

addCommandAsEvent c@(CommandTypeGuard CommandGuard{..}) = do
  target <- getObject commandGuardGuarded
  units <- mapM getUnit commandGuardUnitIds

  let eType = EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
      eventTargetedMilitaryOrderUnits = map unitId units
    , eventTargetedMilitaryOrderType = TargetedMilitaryOrderGuard
    , eventTargetedMilitaryOrderTarget = objectId target
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeFollow CommandFollow{..}) = do
  target <- getObject commandFollowFollowed
  units <- mapM getUnit commandFollowUnitIds

  let eType = EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
      eventTargetedMilitaryOrderUnits = map unitId units
    , eventTargetedMilitaryOrderType = TargetedMilitaryOrderFollow
    , eventTargetedMilitaryOrderTarget = objectId target
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypePatrol CommandPatrol{..}) = do

  units <- mapM getUnit commandPatrolUnitIds

  let eType = EventTypePatrol $ EventPatrol {
      eventPatrolUnits = map unitId units
    , eventPatrolWaypoints = commandPatrolWaypoints
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeBuild CommandBuild{..}) = do
  units <- getUnitsForPlayer commandBuildBuilders commandBuildPlayerId

  let eType = EventTypeBuild $ EventBuild {
      eventBuildBuilders = map unitId units
    , eventBuildPos = commandBuildPos
    , eventBuildingType = getBuildingType commandBuildBuildingType
    , eventBuildBuilding = Nothing
    }
  addRealEvent c (Just commandBuildPlayerId) eType


addCommandAsEvent c@(CommandTypeResearch CommandResearch{..}) = do
  building <- getBuildingForPlayer commandResearchBuildingId commandResearchPlayerId

  let eType = EventTypeResearch $ EventResearch {
      eventResearchBuilding = buildingId building
    , eventResearchTech = normalizeTech commandResearchResearch
    }
  addRealEvent c (Just commandResearchPlayerId) eType

addCommandAsEvent c@(CommandTypeTrain CommandTrain{..}) = do
  building <- getBuilding commandTrainBuildingId
  let eType = EventTypeTrain $ EventTrain {
      eventTrainBuilding = buildingId building
    , eventTrainType = objectTypeToUnitType $ normaliseObjectType commandTrainUnitType
    , eventTrainNumber = commandTrainNumber
    }
  addRealEvent c Nothing eType


addCommandAsEvent c@(CommandTypeStop CommandStop{..}) = do
  objs <- mapM getObject commandStopSelectedIds

  let eType = EventTypeStopGeneral $ EventStopGeneral {
      eventStopSelectedIds = map objectId objs
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeWaypoint CommandWaypoint{..}) = do
  uids <- getSelectedObjectIds commandWaypointSelectedIds commandWaypointPlayerId
  objs <- getObjectsForPlayer uids commandWaypointPlayerId
  let eType = EventTypeWaypoint $ EventWaypoint {
      eventWaypointSelectedObjects = map objectId objs,
      eventWaypointPos = commandWaypointPos
    }
  addRealEvent c (Just commandWaypointPlayerId) eType

addCommandAsEvent c@(CommandTypeRally CommandRally{..}) = do
  targetObj <-
    case (commandRallyTargetObject, commandRallyTargetType) of
      (Nothing, Nothing) -> pure Nothing
      (Just o, Just t) -> fmap Just $ getObjectAsType o t
      (a, b) -> error $ "Rally command with inconsistent targets " ++ show (a,b)

  buildings <- mapM getBuilding commandRallySelectedBuildingIds

  let eType = EventTypeRally $ EventRally {
      eventRallyTargetObject = fmap objectId targetObj,
      eventRallyPos = commandRallyPos,
      eventRallyBuildings = map buildingId buildings
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeDelete CommandDelete{..}) = do
  target <- getObjectForPlayer commandDeleteObjectId (Just commandDeletePlayerId)

  let eType = EventTypeDelete $ EventDelete {
      eventDeleteObjectId = objectId target
    }
  addRealEvent c (Just commandDeletePlayerId) eType

addCommandAsEvent (CommandTypeWall _) = pure ()
addCommandAsEvent (CommandUnparsed _ _) = pure ()
