{-# LANGUAGE UndecidableInstances #-}
module Data.Mgz.Simulate.Events where

import RIO



import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Simulate.Objects
import Data.List.NonEmpty(NonEmpty(..))

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

eventTrainObjectType :: Event -> Maybe ObjectType
eventTrainObjectType Event{..} =
  case eventType of
    EventTypeTrain e -> Just $ eventTrainType e
    _ -> Nothing


data Event =  Event {
  eventId   :: EventId,
  eventTick :: Int,
  eventKind :: EventKind,
  eventPlayerResponsible :: Maybe PlayerId,
  eventType :: EventType,
  eventAssignObjectIds :: [ObjectId] -- this is the earliest event at which these ids could have been assigned
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

data EventVillOnRepairable = EventVillOnRepairable {
  eventVillOnRepairableVills :: [UnitId],
  eventVillOnRepairableType :: NonEmpty EventTypeW,
  eventVillOnRepairableObject :: ObjectId,
  eventVillOnRepairablePos :: Pos
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
  eventBuildingType :: ObjectType,
  eventBuildPos :: Pos,
  eventBuildBuilding :: Maybe BuildingId
}  deriving (Show, Eq, Ord)

eventLinkedBuilding :: Event -> Maybe BuildingId
eventLinkedBuilding e =
  case eventType e of
    EventTypeBuild b -> eventBuildBuilding b
    _ -> Nothing

getSingleEventPos :: Event -> Maybe Pos
getSingleEventPos e =
  case eventType e of
    EventTypeBuild b -> Just $ eventBuildPos b
    EventTypePrimary b -> Just $ eventPrimaryPos b
    EventTypeMove b -> Just $ eventMovePos b
    EventTypeAttack b -> Just $ eventAttackPos b
    EventTypeGather b -> Just $ eventGatherPos b
    EventTypeVillOnRepairable b -> Just $ eventVillOnRepairablePos b
    EventTypeGatherRelic b -> Just $ eventGatherRelicPos b
    EventTypeRally b -> Just $ eventRallyPos b
    EventTypeAttackGround b -> Just $ eventAttackGroundPos b
    _ -> Nothing

getEventBuildPos :: Event -> Pos
getEventBuildPos e =
  case eventType e of
    EventTypeBuild b -> eventBuildPos b
    _ -> error $ "getEventBuildPos only usable for build events"

setEventLinkedBuilding :: Event -> BuildingId -> Event
setEventLinkedBuilding e bid =
  case eventType e of
    EventTypeBuild b -> e{eventType = EventTypeBuild b{eventBuildBuilding = Just bid}}
    EventTypeWall _ -> e -- we don't do this yet
    _ -> error  $ "Could not setEventLinkedBuilding on this kind of event " ++ show e

eventBuildBuildingObjectType :: Event -> Maybe ObjectType
eventBuildBuildingObjectType Event{..} =
  case eventType of
    EventTypeBuild e -> Just $ eventBuildingType e
    EventTypeWall e -> Just $ eventWallBuildingType e
    _ -> Nothing

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
  eventTrainType :: ObjectType,
  eventTrainNumber :: Int,
  eventTrainUnit :: Maybe UnitId,
  eventTrainConsumeWithUnit :: Maybe UnitId
}  deriving (Show, Eq, Ord)

eventTrainBuildingPartial :: Event -> BuildingId
eventTrainBuildingPartial e =
  case eventType e of
    EventTypeTrain b -> eventTrainBuilding b
    _ -> error $ "Only use eventTrainBuildingPartial on event trains, not " ++ show e



eventLinkedUnit :: Event -> Maybe UnitId
eventLinkedUnit e =
  case eventType e of
    EventTypeTrain b -> eventTrainUnit b
    _ -> Nothing

eventConsumedWithUnit :: Event -> Maybe UnitId
eventConsumedWithUnit e =
  case eventType e of
    EventTypeTrain b -> eventTrainConsumeWithUnit b
    _ -> Nothing

setEventConsumedWithUnit :: Event -> UnitId -> Event
setEventConsumedWithUnit e bid =
  case eventType e of
    EventTypeTrain b -> e{eventType = EventTypeTrain b{eventTrainConsumeWithUnit = Just bid}}
    _ -> error  $ "Could not setEventLinkedUnit on this kind of event " ++ show e


setEventLinkedUnit :: Event -> UnitId -> Event
setEventLinkedUnit e bid =
  case eventType e of
    EventTypeTrain b -> e{eventType = EventTypeTrain b{eventTrainUnit = Just bid}}
    _ -> error  $ "Could not setEventLinkedUnit on this kind of event " ++ show e



eventTrainUnitObjectType :: Event -> ObjectType
eventTrainUnitObjectType Event{..} =
  case eventType of
    EventTypeTrain e -> eventTrainType e
    _ -> error " Only use this for events previously validated as train events"

data EventResearch = EventResearch {
  eventResearchBuilding :: BuildingId,
  eventResearchTech :: Tech
}  deriving (Show, Eq, Ord)

eventResearchBuildingMaybe :: Event -> Maybe BuildingId
eventResearchBuildingMaybe e =
  case eventType e of
    EventTypeResearch b -> pure $  eventResearchBuilding b
    _ -> Nothing

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

getEventDeleteObjectId :: Event -> ObjectId
getEventDeleteObjectId e =
  case eventType e of
    EventTypeDelete b -> eventDeleteObjectId b
    _ -> error " Only use this for events previously validated as delete events"

data EventResign = EventResign {
  eventResignPlayerId :: PlayerId
}  deriving (Show, Eq, Ord)

data EventAttackGround = EventAttackGround {
  eventAttackGroundUnitIds :: [UnitId]
, eventAttackGroundPos :: Pos
}  deriving (Show, Eq, Ord)

data EventTribute = EventTribute {
  eventTributeFrom :: PlayerId,
  eventTributeTo :: PlayerId,
  eventTributeResourceKind :: ResourceKind,
  eventTributeAmount :: Float,
  eventTributeTransationFee :: Float
}  deriving (Show, Eq, Ord)

data EventRepair = EventRepair {
  eventRepairRepaired :: ObjectId,
  eventRepairRepairers :: [UnitId]
}  deriving (Show, Eq, Ord)

data EventUngarrison = EventUngarrison {
  eventUngarrisonPos :: Maybe Pos,
  eventUngarrisonType :: Int,
  eventUngarrisonObjectClicked :: Maybe UnitId,
  eventUngarrisonReleasedFrom :: [ObjectId]
}  deriving (Show, Eq, Ord)

data EventToggleGate = EventToggleGate {
  eventToggleGateGate :: BuildingId
}  deriving (Show, Eq, Ord)

data EventGarrison = EventGarrison {
  eventGarrisonTargetId :: ObjectId,
  eventGarrisonGarrisonedUnits :: [UnitId]
}  deriving (Show, Eq, Ord)

data EventPackOrUnpack = EventPackOrUnpack {
  eventPackOrUnpackTrebuchets :: [UnitId],
  eventPackOrUnpackPacked :: Bool
}  deriving (Show, Eq, Ord)

data BuyOrSell =
    Buy
  | Sell
  deriving (Show, Eq, Ord)

data EventUseMarket = EventUseMarket {
  eventUseMarketBuyOrSell :: BuyOrSell,
  eventUseMarketKind :: ResourceKind,
  eventUseMarketAmount :: Int, -- in hundreds
  eventUseMarketMarket :: BuildingId
  }  deriving (Show, Eq, Ord)

data EventDropRelic = EventDropRelic {
  eventDropRelicMonkId :: UnitId
}  deriving (Show, Eq, Ord)

data EventTownBell = EventTownBell {
  eventTownBellTownCenter :: BuildingId,
  eventTownBellActive :: Bool
}  deriving (Show, Eq, Ord)

data EventBackToWork = EventBackToWork {
  eventBackToWorkBuildingId :: BuildingId
}  deriving (Show, Eq, Ord)

data EventWall = EventWall {
      eventWallStartPos :: PosSimple
    , eventWallEndPos :: PosSimple
    , eventWallBuildingType :: ObjectType
    , eventWallBuilders :: [UnitId]
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
  | EventTypeResign EventResign
  | EventTypeAttackGround EventAttackGround
  | EventTypeTribute EventTribute
  | EventTypeRepair EventRepair
  | EventTypeUngarrison EventUngarrison
  | EventTypeToggleGate EventToggleGate
  | EventTypeGarrison EventGarrison
  | EventTypePackOrUnpack EventPackOrUnpack
  | EventTypeUseMarket EventUseMarket
  | EventTypeDropRelic EventDropRelic
  | EventTypeTownBell EventTownBell
  | EventTypeBackToWork EventBackToWork
  | EventTypeWall EventWall
  | EventTypeVillOnRepairable EventVillOnRepairable
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
  referencesObjectIds (EventTypeResign e) = referencesObjectIds e
  referencesObjectIds (EventTypeAttackGround e) = referencesObjectIds e
  referencesObjectIds (EventTypeTribute e) = referencesObjectIds e
  referencesObjectIds (EventTypeRepair e) = referencesObjectIds e
  referencesObjectIds (EventTypeUngarrison e) = referencesObjectIds e
  referencesObjectIds (EventTypeToggleGate e) = referencesObjectIds e
  referencesObjectIds (EventTypeGarrison e) = referencesObjectIds e
  referencesObjectIds (EventTypePackOrUnpack e) = referencesObjectIds e
  referencesObjectIds (EventTypeUseMarket e) = referencesObjectIds e
  referencesObjectIds (EventTypeDropRelic e) = referencesObjectIds e
  referencesObjectIds (EventTypeTownBell e) = referencesObjectIds e
  referencesObjectIds (EventTypeBackToWork e) = referencesObjectIds e
  referencesObjectIds (EventTypeWall e) = referencesObjectIds e
  referencesObjectIds (EventTypeVillOnRepairable e) = referencesObjectIds e

class ReferencesObjectIds a where
  referencesObjectIds :: a -> [ObjectId]

instance {-# OVERLAPPABLE #-}  (EventActingObjects a) => ReferencesObjectIds a where
  referencesObjectIds = eventActingObjects

instance ReferencesObjectIds EventMove where
   referencesObjectIds EventMove{..} = map toObjectId eventMoveUnits

instance ReferencesObjectIds EventAttack where
   referencesObjectIds EventAttack{..} = eventAttackTargetId:eventAttackAttackers

instance ReferencesObjectIds EventGather where
   referencesObjectIds EventGather{..} = eventGatherTargetId:(map toObjectId eventGatherGatherers)

instance ReferencesObjectIds EventVillOnRepairable where
   referencesObjectIds e@EventVillOnRepairable{..} = (toObjectId eventVillOnRepairableObject):eventActingObjects e

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
  | EventTypeWResign
  | EventTypeWAttackGround
  | EventTypeWTribute
  | EventTypeWRepair
  | EventTypeWUngarrison
  | EventTypeWToggleGate
  | EventTypeWGarrison
  | EventTypeWPackOrUnpack
  | EventTypeWUseMarket
  | EventTypeWDropRelic
  | EventTypeWTownBell
  | EventTypeWBackToWork
  | EventTypeWWall
  | EventTypeWDropoff
  | EventTypeWVillOnRepairable
  deriving (Show, Eq, Ord)

eventTypeW :: Event -> EventTypeW
eventTypeW e =
  case eventType e of
    EventTypeMove                  _ -> EventTypeWMove
    EventTypeAttack                _ -> EventTypeWAttack
    EventTypeGather                _ -> EventTypeWGather
    EventTypeGatherRelic           _ -> EventTypeWGatherRelic
    EventTypePrimary               _ -> EventTypeWPrimary
    EventTypeMilitaryDisposition   _ -> EventTypeWMilitaryDisposition
    EventTypeTargetedMilitaryOrder _ -> EventTypeWTargetedMilitaryOrder
    EventTypePatrol                _ -> EventTypeWPatrol
    EventTypeBuild                 _ -> EventTypeWBuild
    EventTypeTrain                 _ -> EventTypeWTrain
    EventTypeResearch              _ -> EventTypeWResearch
    EventTypeStopGeneral           _ -> EventTypeWStopGeneral
    EventTypeWaypoint              _ -> EventTypeWWaypoint
    EventTypeRally                 _ -> EventTypeWRally
    EventTypeDelete                _ -> EventTypeWDelete
    EventTypeResign                _ -> EventTypeWResign
    EventTypeAttackGround          _ -> EventTypeWAttackGround
    EventTypeTribute               _ -> EventTypeWTribute
    EventTypeRepair                _ -> EventTypeWRepair
    EventTypeUngarrison            _ -> EventTypeWUngarrison
    EventTypeToggleGate            _ -> EventTypeWToggleGate
    EventTypeGarrison              _ -> EventTypeWGarrison
    EventTypePackOrUnpack          _ -> EventTypeWPackOrUnpack
    EventTypeUseMarket             _ -> EventTypeWUseMarket
    EventTypeDropRelic             _ -> EventTypeWDropRelic
    EventTypeTownBell              _ -> EventTypeWTownBell
    EventTypeBackToWork            _ -> EventTypeWBackToWork
    EventTypeWall                  _ -> EventTypeWWall
    EventTypeVillOnRepairable        _ -> EventTypeWVillOnRepairable

class EventActingObjects a where
  eventActingObjects :: a -> [ObjectId]

instance EventActingObjects Event where
  eventActingObjects = eventActingObjects . eventType

instance EventActingObjects EventType where
  eventActingObjects (EventTypeMove e) = eventActingObjects e
  eventActingObjects (EventTypeAttack e) = eventActingObjects e
  eventActingObjects (EventTypeGather e) = eventActingObjects e
  eventActingObjects (EventTypeGatherRelic e) = eventActingObjects e
  eventActingObjects (EventTypePrimary e) = eventActingObjects e
  eventActingObjects (EventTypeMilitaryDisposition e) = eventActingObjects e
  eventActingObjects (EventTypeTargetedMilitaryOrder e) = eventActingObjects e
  eventActingObjects (EventTypePatrol e) = eventActingObjects e
  eventActingObjects (EventTypeBuild e) = eventActingObjects e
  eventActingObjects (EventTypeTrain e) = eventActingObjects e
  eventActingObjects (EventTypeResearch e) = eventActingObjects e
  eventActingObjects (EventTypeStopGeneral e) = eventActingObjects e
  eventActingObjects (EventTypeWaypoint e) = eventActingObjects e
  eventActingObjects (EventTypeRally e) = eventActingObjects e
  eventActingObjects (EventTypeDelete e) = eventActingObjects e
  eventActingObjects (EventTypeResign e) = eventActingObjects e
  eventActingObjects (EventTypeAttackGround e) = eventActingObjects e
  eventActingObjects (EventTypeTribute e) = eventActingObjects e
  eventActingObjects (EventTypeRepair e) = eventActingObjects e
  eventActingObjects (EventTypeUngarrison e) = eventActingObjects e
  eventActingObjects (EventTypeToggleGate e) = eventActingObjects e
  eventActingObjects (EventTypeGarrison e) = eventActingObjects e
  eventActingObjects (EventTypePackOrUnpack e) = eventActingObjects e
  eventActingObjects (EventTypeUseMarket e) = eventActingObjects e
  eventActingObjects (EventTypeDropRelic e) = eventActingObjects e
  eventActingObjects (EventTypeTownBell e) = eventActingObjects e
  eventActingObjects (EventTypeBackToWork e) = eventActingObjects e
  eventActingObjects (EventTypeWall e) = eventActingObjects e
  eventActingObjects (EventTypeVillOnRepairable e) = eventActingObjects e

instance EventActingObjects EventMove where
  eventActingObjects EventMove{..} =  map toObjectId eventMoveUnits

instance EventActingObjects EventAttack where
  eventActingObjects EventAttack{..} = map toObjectId eventAttackAttackers

instance EventActingObjects EventGather where
  eventActingObjects EventGather{..} =  map toObjectId eventGatherGatherers

instance EventActingObjects EventVillOnRepairable where
  eventActingObjects EventVillOnRepairable{..} =  map toObjectId eventVillOnRepairableVills

instance EventActingObjects EventGatherRelic where
  eventActingObjects EventGatherRelic{..} =  map toObjectId eventGatherRelicGatherers

instance EventActingObjects EventPrimary where
  eventActingObjects EventPrimary{..} =  map toObjectId eventPrimaryObjects

instance EventActingObjects EventMilitaryDisposition where
  eventActingObjects EventMilitaryDisposition{..} =  map toObjectId eventMilitaryDispositionUnits

instance EventActingObjects EventTargetedMilitaryOrder where
  eventActingObjects EventTargetedMilitaryOrder{..} =  map toObjectId eventTargetedMilitaryOrderUnits

instance EventActingObjects EventPatrol where
  eventActingObjects EventPatrol{..} =  map toObjectId eventPatrolUnits

instance EventActingObjects EventBuild where
  eventActingObjects EventBuild{..} =  map toObjectId eventBuildBuilders

instance EventActingObjects EventTrain where
  eventActingObjects EventTrain{..} = pure $ toObjectId eventTrainBuilding

instance EventActingObjects EventResearch where
  eventActingObjects EventResearch{..} = pure $ toObjectId eventResearchBuilding

instance EventActingObjects EventStopGeneral where
  eventActingObjects EventStopGeneral{..} = eventStopSelectedIds

instance EventActingObjects EventWaypoint where
  eventActingObjects EventWaypoint{..} = eventWaypointSelectedObjects

instance EventActingObjects EventRally where
  eventActingObjects EventRally{..} =  map toObjectId eventRallyBuildings

instance EventActingObjects EventDelete where
  eventActingObjects EventDelete{..} =  [eventDeleteObjectId]

instance EventActingObjects EventResign where
  eventActingObjects EventResign{..} = []

instance EventActingObjects EventAttackGround where
  eventActingObjects EventAttackGround{..} =  map toObjectId eventAttackGroundUnitIds

instance EventActingObjects EventTribute where
  eventActingObjects EventTribute{..} = []

instance EventActingObjects EventRepair where
  eventActingObjects EventRepair{..} = map toObjectId eventRepairRepairers

instance EventActingObjects EventUngarrison where
  eventActingObjects EventUngarrison{..} =  eventUngarrisonReleasedFrom ++ catMaybes [fmap toObjectId eventUngarrisonObjectClicked]

instance EventActingObjects EventToggleGate where
  eventActingObjects EventToggleGate{..} =  [toObjectId eventToggleGateGate]

instance EventActingObjects EventGarrison where
  eventActingObjects EventGarrison{..} = map toObjectId eventGarrisonGarrisonedUnits ++ [eventGarrisonTargetId] -- @team

instance EventActingObjects EventPackOrUnpack where
  eventActingObjects EventPackOrUnpack{..} =  map toObjectId eventPackOrUnpackTrebuchets

instance EventActingObjects EventUseMarket where
  eventActingObjects EventUseMarket{..} =  [toObjectId eventUseMarketMarket]

instance EventActingObjects EventDropRelic where
  eventActingObjects EventDropRelic{..} = [toObjectId eventDropRelicMonkId]

instance EventActingObjects EventTownBell where
  eventActingObjects EventTownBell{..} = [toObjectId eventTownBellTownCenter]

instance EventActingObjects EventBackToWork where
  eventActingObjects EventBackToWork{..} = [toObjectId eventBackToWorkBuildingId]

instance EventActingObjects EventWall where
  eventActingObjects EventWall{..} = map toObjectId eventWallBuilders