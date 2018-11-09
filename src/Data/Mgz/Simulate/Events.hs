module Data.Mgz.Simulate.Events where

import RIO

import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE


import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Simulate.Objects


data EventKind =
    EventKindReal Command -- something that is directly derived from a command
  | EventKindSimulated -- our best guess
  deriving (Show, Eq, Ord)



-- this is our representation of something that happens in the game

eventTechType :: Event -> Maybe Tech
eventTechType Event{..} =
  case eventType of
    EventTypeResearch e -> pure $ eventResearchTech e
    _ -> Nothing

eventTrainObjectType :: Event -> Maybe (NonEmpty ObjectType)
eventTrainObjectType Event{..} =
  case eventType of
    EventTypeTrain e -> objectTypeFromUnitType $ eventTrainType e
    _ -> Nothing


data Event =  Event {
  eventId   :: EventId,
  eventTick :: Int,
  eventKind :: EventKind,
  eventPlayerResponsible :: Maybe PlayerId,
  eventType :: EventType
} deriving (Show, Eq, Ord)

data EventMove = EventMove {
  eventMoveUnits :: [UnitId],
  eventMovePos :: Pos
} deriving (Show, Eq, Ord)

data EventAttack = EventAttack {
  eventAttackAttackers :: [ObjectId],
  eventAttackTargetId :: ObjectId,
  eventAttackPos :: Pos
} deriving (Show, Eq, Ord)

data EventGather = EventGather {
  eventGatherGatherers :: [UnitId],
  eventGatherTargetId :: ObjectId,
  eventGatherPos :: Pos
} deriving (Show, Eq, Ord)

data EventGatherRelic = EventGatherRelic {
  eventGatherRelicGatherers :: [UnitId],
  eventGatherRelicTargetId :: ObjectId,
  eventGatherRelicPos :: Pos
} deriving (Show, Eq, Ord)

data EventPrimary = EventPrimary {
  eventPrimaryObjects :: [ObjectId], -- can be a building!
  eventPrimaryTarget :: ObjectId,
  eventPrimaryPos :: Pos
}  deriving (Show, Eq, Ord)

{-data EventHelpBuildOrRepair = EventHelpBuildOrRepair {
  eventPrimaryObjects :: [ObjectId], -- can be a building!
  eventPrimaryTarget :: ObjectId,
  eventPrimaryPos :: Pos
}  deriving (Show, Eq, Ord)-}

data EventBuild = EventBuild {
  eventBuildBuilders :: [UnitId],
  eventBuildingType :: BuildingType,
  eventBuildPos :: Pos,
  eventBuildBuilding :: Maybe BuildingId
}  deriving (Show, Eq, Ord)

extractEventBuild :: Event -> EventBuild
extractEventBuild e =
  case eventType e of
    EventTypeBuild b -> b
    _ -> error " Could not extract build event "

eventBuildBuildingObjectType :: Event -> ObjectType
eventBuildBuildingObjectType Event{..} =
  case eventType of
    EventTypeBuild e ->
      case fmap NE.toList $ objectTypeFromBuildingType $ eventBuildingType e of
        (Just [x]) -> x
        _ -> error "This should be impossible"
    _ -> error " Only use this for events previously validated as build events"
data MilitaryDisposition =
    MilitaryDispositionStance Int
  | MilitaryDispositionFormation Int
  deriving (Show, Eq, Ord)
data EventMilitaryDisposition = EventMilitaryDisposition {
  eventMilitaryDispositionType :: MilitaryDisposition,
  eventMilitaryDispositionUnits :: [UnitId]
}  deriving (Show, Eq, Ord)

data TargetedMilitaryOrder =
    TargetedMilitaryOrderGuard
  | TargetedMilitaryOrderFollow
  deriving (Show, Eq, Ord)

data EventTargetedMilitaryOrder = EventTargetedMilitaryOrder {
  eventTargetedMilitaryOrderType :: TargetedMilitaryOrder,
  eventTargetedMilitaryOrderUnits :: [UnitId],
  eventTargetedMilitaryOrderTarget :: ObjectId -- this should be unitId maybe? can you guard buildings?
}  deriving (Show, Eq, Ord)


data EventPatrol = EventPatrol {
  eventPatrolUnits :: [UnitId],
  eventPatrolWaypoints :: [Pos]
}  deriving (Show, Eq, Ord)

data EventTrain = EventTrain {
  eventTrainBuilding :: BuildingId,
  eventTrainType :: UnitType,
  eventTrainNumber :: Int
}  deriving (Show, Eq, Ord)

data EventResearch = EventResearch {
  eventResearchBuilding :: BuildingId,
  eventResearchTech :: Tech
}  deriving (Show, Eq, Ord)

data EventStopGeneral = EventStopGeneral {
  eventStopSelectedIds :: [ObjectId]
}  deriving (Show, Eq, Ord)

data EventWaypoint = EventWaypoint {
  eventWaypointSelectedObjects :: [ObjectId],
  eventWaypointPos :: PosSimple
}  deriving (Show, Eq, Ord)

data EventRally = EventRally {
  eventRallyTargetObject :: Maybe ObjectId,
  eventRallyPos :: Pos,
  eventRallyBuildings :: [BuildingId]
}  deriving (Show, Eq, Ord)

data EventDelete = EventDelete {
  eventDeleteObjectId :: ObjectId
}  deriving (Show, Eq, Ord)

data EventType =
    EventTypeMove EventMove
  | EventTypeAttack EventAttack
  | EventTypeGather EventGather
  | EventTypeGatherRelic EventGatherRelic
  | EventTypePrimary EventPrimary
  | EventTypeMilitaryDisposition EventMilitaryDisposition
  | EventTypeTargetedMilitaryOrder EventTargetedMilitaryOrder
  | EventTypePatrol EventPatrol
  | EventTypeBuild EventBuild
  | EventTypeTrain EventTrain
  | EventTypeResearch EventResearch
  | EventTypeStopGeneral EventStopGeneral
  | EventTypeWaypoint EventWaypoint
  | EventTypeRally EventRally
  | EventTypeDelete EventDelete
  deriving (Show, Eq, Ord)


instance ReferencesObjectIds Event where
  referencesObjectIds = referencesObjectIds . eventType

instance ReferencesObjectIds EventType where
  referencesObjectIds (EventTypeMove e) = referencesObjectIds e
  referencesObjectIds (EventTypeAttack e) = referencesObjectIds e
  referencesObjectIds (EventTypeGather e) = referencesObjectIds e
  referencesObjectIds (EventTypeGatherRelic e) = referencesObjectIds e
  referencesObjectIds (EventTypePrimary e) = referencesObjectIds e
  referencesObjectIds (EventTypeMilitaryDisposition e) = referencesObjectIds e
  referencesObjectIds (EventTypeTargetedMilitaryOrder e) = referencesObjectIds e
  referencesObjectIds (EventTypePatrol e) = referencesObjectIds e
  referencesObjectIds (EventTypeBuild e) = referencesObjectIds e
  referencesObjectIds (EventTypeTrain e) = referencesObjectIds e
  referencesObjectIds (EventTypeResearch e) = referencesObjectIds e
  referencesObjectIds (EventTypeStopGeneral e) = referencesObjectIds e
  referencesObjectIds (EventTypeWaypoint e) = referencesObjectIds e
  referencesObjectIds (EventTypeRally e) = referencesObjectIds e
  referencesObjectIds (EventTypeDelete e) = referencesObjectIds e


class ReferencesObjectIds a where
  referencesObjectIds :: a -> [ObjectId]

instance ReferencesObjectIds EventMove where
   referencesObjectIds EventMove{..} = map toObjectId eventMoveUnits

instance ReferencesObjectIds EventAttack where
   referencesObjectIds EventAttack{..} = eventAttackTargetId:eventAttackAttackers

instance ReferencesObjectIds EventGather where
   referencesObjectIds EventGather{..} = eventGatherTargetId:(map toObjectId eventGatherGatherers)

instance ReferencesObjectIds EventGatherRelic where
   referencesObjectIds EventGatherRelic{..} = eventGatherRelicTargetId:(map toObjectId eventGatherRelicGatherers)

instance ReferencesObjectIds EventPrimary where
   referencesObjectIds EventPrimary{..} = eventPrimaryTarget:eventPrimaryObjects

instance ReferencesObjectIds EventBuild where
   referencesObjectIds EventBuild{..} = map toObjectId eventBuildBuilders

instance ReferencesObjectIds EventMilitaryDisposition where
   referencesObjectIds EventMilitaryDisposition{..} = map toObjectId eventMilitaryDispositionUnits

instance ReferencesObjectIds EventTargetedMilitaryOrder where
   referencesObjectIds EventTargetedMilitaryOrder{..} = eventTargetedMilitaryOrderTarget:(map toObjectId eventTargetedMilitaryOrderUnits)

instance ReferencesObjectIds EventPatrol where
   referencesObjectIds EventPatrol{..} = map toObjectId eventPatrolUnits

instance ReferencesObjectIds EventTrain where
   referencesObjectIds EventTrain{..} = [toObjectId eventTrainBuilding]

instance ReferencesObjectIds EventResearch where
   referencesObjectIds EventResearch{..} = [toObjectId eventResearchBuilding]

instance ReferencesObjectIds EventStopGeneral where
   referencesObjectIds EventStopGeneral{..} = eventStopSelectedIds

instance ReferencesObjectIds EventWaypoint where
   referencesObjectIds EventWaypoint{..} = eventWaypointSelectedObjects

instance ReferencesObjectIds EventRally where
   referencesObjectIds EventRally{..} = catMaybes [eventRallyTargetObject] ++ map toObjectId eventRallyBuildings

instance ReferencesObjectIds EventDelete where
   referencesObjectIds EventDelete{..} = [eventDeleteObjectId]


-- witness the type of an event
data EventTypeW =
    EventTypeWMove
  | EventTypeWAttack
  | EventTypeWGather
  | EventTypeWGatherRelic
  | EventTypeWPrimary
  | EventTypeWMilitaryDisposition
  | EventTypeWTargetedMilitaryOrder
  | EventTypeWPatrol
  | EventTypeWBuild
  | EventTypeWTrain
  | EventTypeWResearch
  | EventTypeWStopGeneral
  | EventTypeWWaypoint
  | EventTypeWRally
  | EventTypeWDelete
  deriving (Show, Eq, Ord)

eventTypeW :: Event -> EventTypeW
eventTypeW e =
  case eventType e of
    EventTypeMove _ -> EventTypeWMove
    EventTypeAttack _ -> EventTypeWAttack
    EventTypeGather _ -> EventTypeWGather
    EventTypeGatherRelic _ -> EventTypeWGatherRelic
    EventTypePrimary _ -> EventTypeWPrimary
    EventTypeMilitaryDisposition _ -> EventTypeWMilitaryDisposition
    EventTypeTargetedMilitaryOrder _ -> EventTypeWTargetedMilitaryOrder
    EventTypePatrol _ -> EventTypeWPatrol
    EventTypeBuild _ -> EventTypeWBuild
    EventTypeTrain _ -> EventTypeWTrain
    EventTypeResearch _ -> EventTypeWResearch
    EventTypeStopGeneral _ -> EventTypeWStopGeneral
    EventTypeWaypoint _ -> EventTypeWWaypoint
    EventTypeRally _ -> EventTypeWRally
    EventTypeDelete _ -> EventTypeWDelete

eventActingObjectsIdx :: Event -> [ObjectId]
eventActingObjectsIdx e =
  case eventType e of
    EventTypeMove EventMove{..} -> map toObjectId eventMoveUnits
    EventTypeAttack EventAttack{..} -> map toObjectId eventAttackAttackers
    EventTypeGather EventGather{..} -> map toObjectId eventGatherGatherers
    EventTypeGatherRelic EventGatherRelic{..} -> map toObjectId eventGatherRelicGatherers
    EventTypePrimary EventPrimary{..} -> map toObjectId eventPrimaryObjects
    EventTypeMilitaryDisposition EventMilitaryDisposition{..} -> map toObjectId eventMilitaryDispositionUnits
    EventTypeTargetedMilitaryOrder EventTargetedMilitaryOrder{..} -> map toObjectId eventTargetedMilitaryOrderUnits
    EventTypePatrol EventPatrol{..} -> map toObjectId eventPatrolUnits
    EventTypeBuild EventBuild{..} -> map toObjectId eventBuildBuilders
    EventTypeTrain EventTrain{..} ->  pure $ toObjectId eventTrainBuilding
    EventTypeResearch EventResearch{..} -> pure $ toObjectId eventResearchBuilding
    EventTypeStopGeneral EventStopGeneral{..} -> eventStopSelectedIds
    EventTypeWaypoint EventWaypoint{..} -> eventWaypointSelectedObjects
    EventTypeRally EventRally{..} -> map toObjectId eventRallyBuildings
    EventTypeDelete EventDelete{..} -> []
