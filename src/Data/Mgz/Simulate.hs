{-# LANGUAGE TemplateHaskell    #-}

module Data.Mgz.Simulate where

import RIO
import Voobly.TH

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import qualified RIO.List as L
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.IxSet.Typed as IxSet
import  Data.IxSet.Typed (IxSet, Indexable, IsIndexOf)
import Control.Monad.State.Strict
import Data.Proxy(Proxy(..))
import qualified Data.Text.Format as F
import qualified Data.Text.Buildable as F.Buildable
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified RIO.HashMap as HM

mNonEmpty :: Maybe a -> Maybe (NonEmpty a)
mNonEmpty Nothing = Nothing
mNonEmpty (Just a) = pure $ a :| []


elemNonEmpty :: Eq a => a -> NonEmpty a -> Bool
elemNonEmpty a ne = a `elem` NE.toList ne

newtype ObjectId = ObjectId {objectIdToInt :: Int} deriving (Show, Eq, Ord) -- object id from rec file - we don't know anything about it!

newtype ObjectUnit = ObjectUnit Object -- just for the types
newtype ObjectBuilding = ObjectBuilding Object -- just for the types

newtype UnitId = UnitId ObjectId deriving (Show, Eq, Ord)
newtype BuildingId = BuildingId ObjectId deriving (Show, Eq, Ord)
newtype ResourceId = ResourceId ObjectId deriving (Show, Eq, Ord)

newtype PlayerId = PlayerId Int deriving (Show, Eq, Ord) -- 1,2 etc - same as the rec file
newtype EventId = EventId Int deriving (Show, Eq, Ord) -- sequence

class ToObjectId a where
  toObjectId :: a -> ObjectId

instance ToObjectId ObjectId where
  toObjectId = id


instance ToObjectId UnitId where
  toObjectId (UnitId i) = i

instance ToObjectId BuildingId where
  toObjectId (BuildingId i) = i

instance ToObjectId ResourceId where
  toObjectId (ResourceId i) = i



asUnit :: Object -> ObjectUnit
asUnit o =
  if objectTypeW o == ObjectTypeWUnit
    then ObjectUnit o
    else error "Could not get object asUnit"

asBuilding :: Object -> ObjectBuilding
asBuilding o =
  if objectTypeW o == ObjectTypeWBuilding
    then ObjectBuilding o
    else error "Could not get object asBuilding"


unitId :: ObjectUnit -> UnitId
unitId (ObjectUnit o) = UnitId . objectId $ o


buildingId :: ObjectBuilding -> BuildingId
buildingId (ObjectBuilding o) = BuildingId . objectId $ o

data UnitType =
    UnitTypeUnknown
  | UnitTypeVillager
  | UnitTypeMilitary MilitaryType
  | UnitTypeOther ObjectType
  deriving (Show, Eq, Ord)

data MilitaryType =
    MilitaryTypeUnknown
  | MilitaryTypeKnown ObjectType
  | MilitaryTypeOneOf (NonEmpty ObjectType)
  deriving (Show, Eq, Ord)

objectTypeFromUnitType :: UnitType -> Maybe (NonEmpty ObjectType)
objectTypeFromUnitType (UnitTypeMilitary m) = objectTypeFromMilitaryType m
objectTypeFromUnitType (UnitTypeOther o) = NE.nonEmpty [o]
objectTypeFromUnitType UnitTypeVillager = NE.nonEmpty [OT_Villager]
objectTypeFromUnitType UnitTypeUnknown = Nothing

objectTypeFromMilitaryType :: MilitaryType -> Maybe (NonEmpty ObjectType)
objectTypeFromMilitaryType MilitaryTypeUnknown = Nothing
objectTypeFromMilitaryType (MilitaryTypeKnown o) = NE.nonEmpty [o]
objectTypeFromMilitaryType (MilitaryTypeOneOf a) = pure $ a

data BuildingType =
    BuildingTypeUnknown
  | BuildingTypeKnown ObjectType
  | BuildingTypeOneOf (NonEmpty ObjectType)
  deriving (Show, Eq, Ord)

objectTypeFromBuildingType :: BuildingType -> Maybe (NonEmpty ObjectType)
objectTypeFromBuildingType BuildingTypeUnknown = Nothing
objectTypeFromBuildingType (BuildingTypeKnown m) = NE.nonEmpty [m]
objectTypeFromBuildingType (BuildingTypeOneOf o) = pure o

getBuildingType :: Int -> BuildingType
getBuildingType i = BuildingTypeKnown $ normaliseObjectType i

isVillagerType :: ObjectType -> Bool
isVillagerType OT_Villager = True
isVillagerType _ = False

isNotVillagerOrMilitary :: ObjectType -> Bool
isNotVillagerOrMilitary OT_Sheep = True
isNotVillagerOrMilitary _ = False

objectTypeToUnitType :: ObjectType -> UnitType
objectTypeToUnitType t =
  if isVillagerType t
    then UnitTypeVillager
    else if isNotVillagerOrMilitary t
     then UnitTypeOther t
     else UnitTypeMilitary . MilitaryTypeKnown $ t

data EventKind =
    EventKindReal Command -- something that is directly derived from a command
  | EventKindSimulated -- our best guess
  deriving (Show, Eq, Ord)



-- this is our representation of something that happens in the game


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
  eventAttackAttackers :: [UnitId],
  eventAttackTargetId :: ObjectId,
  eventAttackPos :: Pos
} deriving (Show, Eq, Ord)

data EventPrimary = EventPrimary {
  eventPrimaryObjects :: [ObjectId], -- can be a building!
  eventPrimaryTarget :: ObjectId,
  eventPrimaryPos :: Pos
}  deriving (Show, Eq, Ord)

data EventBuild = EventBuild {
  eventBuildBuilders :: [UnitId],
  eventBuildingType :: BuildingType,
  eventBuildPos :: Pos,
  eventBuildBuilding :: Maybe BuildingId
}  deriving (Show, Eq, Ord)


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

data EventType =
    EventTypeMove EventMove
  | EventTypeAttack EventAttack
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
  deriving (Show, Eq, Ord)

data Unit = Unit {
  unitType :: UnitType
} deriving (Show, Eq, Ord)

data Building = Building {
  buildingType :: BuildingType,
  buildingPos :: Maybe Pos
} deriving (Show, Eq, Ord)




data Resource = Resource {
  resourceKind :: ResourceKind,
  resourceType :: ObjectType,
  resourcePos :: Pos
} deriving (Show, Eq, Ord)

data ObjectInfo =
    ObjectInfoUnit Unit
  | ObjectInfoBuilding Building
  | ObjectInfoResource Resource
  | ObjectInfoUnknown (Maybe ObjectType)
  deriving (Show, Eq, Ord)

-- witness the type of an object
data ObjectTypeW =
    ObjectTypeWUnit
  | ObjectTypeWBuilding
  | ObjectTypeWResource
  | ObjectTypeWUnknown
  deriving (Show, Eq, Ord)

objectTypeW :: Object -> ObjectTypeW
objectTypeW o =
  case objectInfo o of
    ObjectInfoUnit _ -> ObjectTypeWUnit
    ObjectInfoBuilding _ -> ObjectTypeWBuilding
    ObjectInfoResource _ -> ObjectTypeWResource
    ObjectInfoUnknown _ -> ObjectTypeWUnknown

buildingTypeIdx :: Object -> Maybe BuildingType
buildingTypeIdx Object{..} =
  case objectInfo of
    ObjectInfoBuilding u -> pure $ buildingType u
    _ -> Nothing

unitTypeIdx :: Object -> Maybe UnitType
unitTypeIdx Object{..} =
  case objectInfo of
    ObjectInfoUnit u -> pure $ unitType u
    _ -> Nothing

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

buildingObjectPos :: Object -> Maybe Pos
buildingObjectPos Object{..} =
  case objectInfo of
    ObjectInfoBuilding b -> buildingPos b
    _ -> Nothing


data Object = Object {
  objectId :: ObjectId,
  objectPlayer :: Maybe PlayerId,
  objectInfo :: ObjectInfo
} deriving (Show, Eq, Ord)


class HasObjectType a where
  toObjectType :: a -> Maybe (NonEmpty ObjectType)
  setObjectType :: a -> ObjectType -> a

instance HasObjectType Object where
  toObjectType o =
    case objectInfo o of
      ObjectInfoUnit u -> toObjectType u
      ObjectInfoBuilding u -> toObjectType u
      ObjectInfoResource u -> toObjectType u
      ObjectInfoUnknown u -> mNonEmpty u
  setObjectType o t =
    let ni =
          case objectInfo o of
            ObjectInfoUnit u -> ObjectInfoUnit $ setObjectType u t
            ObjectInfoBuilding u -> ObjectInfoBuilding $ setObjectType u t
            ObjectInfoResource u -> ObjectInfoResource $ setObjectType u t
            ObjectInfoUnknown _ -> ObjectInfoUnknown $ Just t
    in o{objectInfo = ni}

instance HasObjectType Unit where
  toObjectType u = objectTypeFromUnitType (unitType u)
  setObjectType u t = u{unitType = objectTypeToUnitType t}

instance HasObjectType Resource where
  toObjectType u = NE.nonEmpty [resourceType u]
  setObjectType _ _ = error "All resources should have a type from the beginning"


instance HasObjectType Building where
  toObjectType u = objectTypeFromBuildingType (buildingType u)
  setObjectType u t = u{buildingType = BuildingTypeKnown t}


data MapObject = MapObject {
  mapObjectType :: ObjectType,
  mapObjectOwner :: PlayerId
} deriving (Show, Eq, Ord)

data MapTile = MapTile {
  mapTileX :: Int,
  mapTileY :: Int,
  mapTileResource :: [MapObject]
} deriving (Show, Eq, Ord)


-- witness the type of an event
data EventTypeW =
    EventTypeWMove
  | EventTypeWAttack
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
  deriving (Show, Eq, Ord)

eventTypeW :: Event -> EventTypeW
eventTypeW e =
  case eventType e of
    EventTypeMove _ -> EventTypeWMove
    EventTypeAttack _ -> EventTypeWAttack
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

eventActingObjectsIdx :: Event -> [ObjectId]
eventActingObjectsIdx e =
  case eventType e of
    EventTypeMove EventMove{..} -> map toObjectId eventMoveUnits
    EventTypeAttack EventAttack{..} -> map toObjectId eventAttackAttackers
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



makeSimpleIxSet "ObjectSet" ''Object ['objectId, 'objectTypeW, 'unitTypeIdx, 'buildingTypeIdx]
makeSimpleIxSet "EventSet" ''Event ['eventId, 'eventTypeW, 'eventActingObjectsIdx, 'eventPlayerResponsible]
makeSimpleIxSet "MapTileSet" ''MapTile ['mapTileX, 'mapTileY]

ixsetGetIn :: (Indexable ixs a, IsIndexOf ix ixs) => [ix] -> IxSet ixs a  -> IxSet ixs a
ixsetGetIn = flip (IxSet.@+)

getObjectSet :: Sim ObjectSet
getObjectSet = fmap (objects . gameState) get

getEventSet :: Sim EventSet
getEventSet = fmap (events . gameState) get

data GameState = GameState {
  objects :: ObjectSet,
  events :: EventSet,
  mapTiles :: MapTileSet
} deriving Show

simulate :: HasLogFunc env => RecInfo -> RIO env GameState
simulate RecInfo{..} = do
  logInfo "Start simulating"
  logInfo "Placing map objects"
  let initialMap = IxSet.fromList $ map (\t -> MapTile (tilePositionX t) (tilePositionY t) []) (headerTiles recInfoHeader)
  let initialGS = GameState IxSet.empty IxSet.empty initialMap

  let sWithMap = gameState $ execState (placeMapObjects recInfoHeader) (SimState 0 initialGS HM.empty)

  logInfo "Building base events"

{-  (flip mapM) recInfoOps $ \o -> do
    case o of
      OpTypeCommand c -> do
        case c of
          CommandTypePrimary CommandPrimary{..} -> traceM $ displayShowT commandPrimaryPlayerId <> ": Primary " <> displayShowT commandPrimaryUnitIds
          CommandTypeMove CommandMove{..} -> traceM $ displayShowT commandMovePlayerId <> ": Move " <> displayShowT commandMoveUnitIds
          CommandTypeWaypoint CommandWaypoint{..} -> traceM $ displayShowT commandWaypointPlayerId <> ": Waypoint " <> displayShowT commandWaypointSelectedIds
          _ -> pure ()
      _ -> pure ()
  error "sadf s"-}
  let sBasic = gameState $ execState (mapM buildBasicEvents recInfoOps) (SimState 0 sWithMap HM.empty)
  logInfo $ "Total events: " <> displayShow (IxSet.size . events $ sBasic)

  logInfo "Making simple inferences"
  let sWithSimpleInferences = gameState $ execState makeSimpleInferences (SimState 0 sBasic HM.empty)




  pure $ sWithSimpleInferences


placeMapObjects :: Header -> Sim ()
placeMapObjects h = do
  void $ (flip mapM) (headerPlayers h) $ \PlayerInfo{..} -> do
    mapM (placePlayerObject playerInfoNumber) playerInfoObjects

placePlayerObject :: Int -> ObjectRaw -> Sim ()
{-placePlayerObject _ ObjectRaw{..} =
  case (objectRawPosX, objectRawPosY) of
    (Just x, Just y) -> do
      traceM $ (displayShowT $ normaliseObjectType objectRawUnitId) <> " at " <> displayShowT x <> ", " <> displayShowT y
      let ot = normaliseObjectType objectRawUnitId

    _ -> pure ()-}

placePlayerObject _ _ = pure ()

type Ticks = Int
type LastUsedIds = HM.HashMap Int [Int]
data SimState = SimState {
  ticks :: Ticks,
  gameState :: GameState,
  lastUsedIds :: LastUsedIds
}

type Sim a = State SimState a

makeSimpleInferences :: Sim ()
makeSimpleInferences = do
  unknownUnits <- fmap (IxSet.getEQ (Just UnitTypeUnknown)) $ getObjectSet
  void $ mapM inferUnitType $ IxSet.toList unknownUnits

  unknownBuildings <- fmap (IxSet.getEQ (Just BuildingTypeUnknown)) $ getObjectSet
  void $ mapM inferBuildingType $ IxSet.toList unknownBuildings

  unplayerEvents <- fmap (IxSet.getEQ (Nothing :: Maybe PlayerId)) $ getEventSet
  void $ mapM inferPlayerForEvent $ IxSet.toList unplayerEvents

  pure ()
  where
    inferPlayerForEvent :: Event -> Sim ()
    inferPlayerForEvent e = do
      objs <- fmap catMaybes $  mapM lookupObject $ eventActingObjectsIdx e
      let ps = L.nub . catMaybes $ map objectPlayer objs
      case ps of
        [] -> pure ()
        [x] -> void $ updateEvent e{eventPlayerResponsible = Just x}
        xs -> error $ "Multiple player owners for units in single event" ++ show xs

    inferUnitType :: Object -> Sim ()
    inferUnitType o = do
      villagerEvents <- fmap (ixsetGetIn [EventTypeWBuild]  . IxSet.getEQ (objectId o)) $ getEventSet
      if IxSet.size villagerEvents > 0
        then void $ updateObject o{objectInfo = ObjectInfoUnit $ Unit UnitTypeVillager}
        else do
          militaryEvents <- fmap (ixsetGetIn [EventTypeWPatrol, EventTypeWMilitaryDisposition, EventTypeWTargetedMilitaryOrder]  . IxSet.getEQ (objectId o)) $ getEventSet
          if IxSet.size militaryEvents > 0
            then void $ updateObject o{objectInfo = ObjectInfoUnit $ Unit (UnitTypeMilitary MilitaryTypeUnknown)}
            else pure ()

    inferBuildingType :: Object -> Sim ()
    inferBuildingType o = do
      techEvents <-  fmap (IxSet.toList . ixsetGetIn [EventTypeWResearch]  . IxSet.getEQ (objectId o)) $ getEventSet
      let utsFromTechs = L.nub . concat . catMaybes $ map ((flip HM.lookup) techToBuildingMap) $ L.nub . catMaybes $ map eventTechType techEvents
      trainEvents <- fmap (IxSet.toList . ixsetGetIn [EventTypeWTrain]  . IxSet.getEQ (objectId o)) $ getEventSet
      let utsFromTrainedUnits = L.nub . concat . catMaybes $ map ((flip HM.lookup) trainUnitToBuildingMap) $ L.nub . concat . map NE.toList . catMaybes $ map eventTrainObjectType trainEvents

      case L.nub $ concat [utsFromTechs, utsFromTrainedUnits] of
        [] -> pure ()
        [x] -> void $ updateObject o{objectInfo = ObjectInfoBuilding $ Building (BuildingTypeKnown x) (buildingObjectPos o)}
        x:xs -> void $ updateObject o{objectInfo = ObjectInfoBuilding $ Building (BuildingTypeOneOf $ x :| xs) (buildingObjectPos o)}



buildBasicEvents :: Op -> Sim ()
buildBasicEvents (OpTypeSync OpSync{..}) = modify' (\ss -> ss{ticks = ticks ss + opSyncTime})
buildBasicEvents (OpTypeCommand cmd) =  addCommandAsEvent cmd
buildBasicEvents _ = pure ()


addCommandAsEvent :: Command -> Sim ()
addCommandAsEvent c@(CommandTypePrimary CommandPrimary{..}) = do
  target <- getObject commandPrimaryTargetId
  uids <- getSelectedObjectIds commandPrimaryUnitIds commandPrimaryPlayerId
  objs <- getObjectsForPlayer uids commandPrimaryPlayerId
  let eType = EventTypePrimary $ EventPrimary {
      eventPrimaryObjects = map objectId objs
    , eventPrimaryTarget = objectId target
    , eventPrimaryPos = commandPrimaryPos
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


addCommandAsEvent _ = pure ()

getSelectedObjectIds :: EitherInheritOrIds -> Int -> Sim [Int]
getSelectedObjectIds (Left ()) pId = do
  m <- fmap lastUsedIds $ get
  case HM.lookup pId m of
    Nothing -> error $ "Expected to find inheritable ids for " ++ show pId ++ " but got nothing"
    Just xs -> pure xs
getSelectedObjectIds (Right xs) pId = modify' (\ss -> ss{lastUsedIds = HM.insert pId xs (lastUsedIds ss)}) >> pure xs


getObjectAsType :: Int -> Int -> Sim Object
getObjectAsType i ti = do
  o <- getObjectForPlayer i Nothing
  let t = normaliseObjectType ti
  case toObjectType o of
    Nothing -> updateObject $ setObjectType o t
    Just ots ->
      if t `elemNonEmpty` ots
        then updateObject $ setObjectType o t
        else error $ "Mismatch in object types"


getObject :: Int -> Sim Object
getObject i = getObjectForPlayer i Nothing

getObjectForPlayer :: Int -> Maybe Int -> Sim Object
getObjectForPlayer i mpId = do
  mO <- lookupObject (ObjectId i)
  case mO of
    Just o ->
      case mpId of
        Nothing -> pure o
        Just p -> do
          let pId = PlayerId p
          case objectPlayer o of
            Nothing -> updateObject o{objectPlayer = Just pId}
            Just pId' | pId' == pId -> pure o
                      | otherwise -> error "PlayerId CHANGED - stolen sheep or conversion? "

    Nothing -> do
      let o = Object {
                objectId = ObjectId i
              , objectPlayer = fmap PlayerId mpId
              , objectInfo = ObjectInfoUnknown Nothing
              }
      updateObject o

getUnitsForPlayer :: [Int] -> Int -> Sim [ObjectUnit]
getUnitsForPlayer us i = mapM ((flip getUnitForPlayer) i) us

getObjectsForPlayer :: [Int] -> Int -> Sim [Object]
getObjectsForPlayer us i = mapM ((flip getObjectForPlayer) (Just i)) us


getUnit :: Int -> Sim ObjectUnit
getUnit i = do
  when (i == 2444) $ error "THIS IS WHERE"

  o <- getObject i
  fmap asUnit $ convertObj o ObjectTypeWUnit

getUnitForPlayer :: Int -> Int -> Sim ObjectUnit
getUnitForPlayer i pId = do
  when (i == 2444) $ error "THIS IS WHERE"
  o <- getObjectForPlayer i (Just pId)
  fmap asUnit $ convertObj o ObjectTypeWUnit


getBuilding :: Int -> Sim ObjectBuilding
getBuilding i = do
  o <- getObject i
  fmap asBuilding $ convertObj o ObjectTypeWBuilding

getBuildingForPlayer :: Int -> Int -> Sim ObjectBuilding
getBuildingForPlayer i pId = do
  o <- getObjectForPlayer i (Just pId)
  fmap asBuilding $ convertObj o ObjectTypeWBuilding


convertObj :: Object -> ObjectTypeW -> Sim Object
convertObj o w =
  case objectTypeW o of
    ObjectTypeWUnknown -> do
      let oInfo = case w of
            ObjectTypeWUnit -> ObjectInfoUnit $ Unit UnitTypeUnknown
            ObjectTypeWBuilding -> ObjectInfoBuilding $ Building BuildingTypeUnknown Nothing
            ObjectTypeWResource -> error "Can't use for the w resource"
            ObjectTypeWUnknown -> objectInfo o
          newO = o{objectInfo = oInfo}
      updateObject newO
    ow | ow == w -> pure o
       | otherwise -> error $  "Could not coerce " ++ show o ++ " to " ++ show w

lookupObject :: (ToObjectId a ) => a -> Sim (Maybe Object)
lookupObject i = do
  ixset <- fmap (objects . gameState) $ get
  pure $ IxSet.getOne $ IxSet.getEQ (toObjectId i) ixset




updateObject :: Object -> Sim Object
updateObject o = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{objects = IxSet.updateIx (objectId o) o (objects gs)}}
  pure o


updateEvent :: Event -> Sim Event
updateEvent e = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{events = IxSet.updateIx (eventId e) e (events gs)}}
  pure e

addRealEvent :: Command -> Maybe Int -> EventType -> Sim ()
addRealEvent c mP et = modify' $ \ss ->
  let gs = gameState ss
      e = Event {
            eventId = EventId $ IxSet.size (events gs) + 1
          , eventTick = ticks ss
          , eventKind = EventKindReal c
          , eventPlayerResponsible = fmap PlayerId mP
          , eventType = et
          }
  in ss{gameState = gs{events = IxSet.insert e (events gs)}}






replay :: HasLogFunc env => GameState -> RIO env ()
replay gs = do
  logInfo "Rendering to file"

  let r = evalState renderEvents (SimState 0 gs HM.empty)


  liftIO $ TL.writeFile "/code/voobly-scraper/simHistory" $  r
  logInfo "Render done"



renderEvents :: Sim TL.Text
renderEvents = do
  ss <- get
  t <- mapM renderEvent $ IxSet.toAscList (Proxy :: Proxy EventId) $ events (gameState ss)
  pure $ TL.intercalate "\n" $ map TL.toLazyText t

renderEvent :: Event -> Sim  TL.Builder
renderEvent Event{..} = do
  d <- detail
  p <- renderPlayer eventPlayerResponsible
  pure $ simOrReal <> " " <> rPad 9 eventTick <> rPad 12 p <> d
  where

    detail :: Sim  TL.Builder
    detail =
      case eventType of
        (EventTypePrimary (EventPrimary{..})) -> do
          t <- renderObject eventPrimaryTarget
          u <- renderObjects eventPrimaryObjects
          pure $ "Primaried " <> t <> " with " <> u <> " at " <> renderPos eventPrimaryPos
        (EventTypeMove (EventMove{..})) -> do
          u <- renderUnits eventMoveUnits
          pure $ "Moved " <> u <> " to " <> renderPos eventMovePos
        (EventTypeMilitaryDisposition (EventMilitaryDisposition{..})) -> do
          u <- renderUnits eventMilitaryDispositionUnits
          let (mt, tos) = case eventMilitaryDispositionType of
                         MilitaryDispositionStance i -> ("Stance", displayShowB i)
                         MilitaryDispositionFormation i -> ("Formation", displayShowB i)
          pure $ "Changed " <> mt <> " of " <> u <> " to " <> tos
        (EventTypeTargetedMilitaryOrder (EventTargetedMilitaryOrder{..})) -> do
          u <- renderUnits eventTargetedMilitaryOrderUnits
          t <- renderObject eventTargetedMilitaryOrderTarget

          let v = case eventTargetedMilitaryOrderType of
                    TargetedMilitaryOrderGuard -> "Guarded"
                    TargetedMilitaryOrderFollow -> "Followed"
          pure $ v <> " " <> t <> " with " <> u

        (EventTypePatrol (EventPatrol{..})) -> do
          u <- renderUnits eventPatrolUnits

          pure $ "Patrolled " <> u <> " to " <> (displayShowB . length $ eventPatrolWaypoints) <> " waypoints"
        (EventTypeBuild (EventBuild{..})) -> do
          u <- renderUnits eventBuildBuilders
          pure $ "Placed " <> displayShowB eventBuildingType <> " at " <> renderPos eventBuildPos <> " with " <> u
        (EventTypeResearch (EventResearch{..})) -> do
          t <- renderObject eventResearchBuilding
          pure $ "Researched " <> displayShowB eventResearchTech <> " at " <> t
        (EventTypeTrain (EventTrain{..})) -> do
          t <- renderObject eventTrainBuilding
          pure $ "Trained " <> displayShowB eventTrainNumber <> " " <> displayShowB eventTrainType  <> "s at " <> t
        (EventTypeStopGeneral (EventStopGeneral{..})) -> do
          u <- renderObjects eventStopSelectedIds
          pure $ "Stopped " <> u
        (EventTypeWaypoint (EventWaypoint{..})) -> do
          u <- renderObjects eventWaypointSelectedObjects
          pure $ "Waypointed " <> u <> " to " <> renderPosSimple eventWaypointPos
        (EventTypeRally (EventRally{..})) -> do
          b <- renderObjects eventRallyBuildings
          case eventRallyTargetObject of
            Nothing -> pure $ "Rallied " <> b <> " to map position " <> renderPos eventRallyPos
            Just t -> do
              tr <- renderObject t
              pure $ "Rallied " <> b <> " to " <> tr <> " at map position " <> renderPos eventRallyPos
        _ ->  pure $ "Some event"

    simOrReal :: TL.Builder
    simOrReal =
      case eventKind of
        EventKindReal _ -> "R"
        EventKindSimulated -> "S"
    rPad ::  (F.Buildable.Buildable a) => Int -> a-> TL.Builder
    rPad i a = F.right i ' ' a

renderPlayer :: Maybe PlayerId -> Sim TL.Builder
renderPlayer Nothing = pure "Unknown"
renderPlayer (Just (PlayerId i)) = pure $ "P" <> displayShowB i

renderPos :: Pos -> TL.Builder
renderPos (Pos x y) = "(" <> displayShowB x <> ", " <> displayShowB y <> ")"

renderPosSimple :: PosSimple -> TL.Builder
renderPosSimple (PosSimple x y) = "(" <> displayShowB x <> ", " <> displayShowB y <> ")"

buildLText :: TL.Text -> TL.Builder
buildLText = F.Buildable.build

renderObject :: (ToObjectId a) => a -> Sim TL.Builder
renderObject oid = do
  m <- lookupObject oid
  case m of
    Nothing -> pure $ "OBJECT NOT FOUND (" <> (displayShowB . objectIdToInt . toObjectId $ oid) <> ")"
    Just Object{..} -> do

      t <- case objectInfo of
                ObjectInfoUnit a -> renderUnit a
                ObjectInfoBuilding a -> renderBuilding a
                ObjectInfoResource a -> renderResource a
                ObjectInfoUnknown t ->  pure $ maybe "Unknown object" (displayShowB) t
      belong <- renderPlayer objectPlayer
      pure $ "a " <> t <> " belonging to " <> belong

renderObjectType :: (ToObjectId a) => a -> Sim TL.Builder
renderObjectType oid = do
  m <- lookupObject oid
  case m of
    Nothing -> pure $ "NOT FOUND"
    Just Object{..} -> do
      t <- case objectInfo of
            ObjectInfoUnit a -> renderUnit a
            ObjectInfoBuilding a -> renderBuilding a
            ObjectInfoResource a -> renderResource a
            ObjectInfoUnknown t -> pure $ maybe "Unknown" (displayShowB) t
      p <- renderPlayer objectPlayer
      pure $ p <> " " <> t <> "(" <> (displayShowB . objectIdToInt . toObjectId $ oid) <> ")"

renderUnits :: (ToObjectId a) => [a] -> Sim TL.Builder
renderUnits = renderObjects
renderObjects :: (ToObjectId a) => [a] -> Sim TL.Builder
renderObjects [] = pure "NO UNITS"
renderObjects is = do
  us <- mapM renderObjectType is
  let ts = map TL.toLazyText us
      types = L.sort . L.nub $ ts
      rs = map (\t -> (displayShowTL $ length (filter ((==) t) ts)) <> " " <> t <> "s") types
  pure $ F.Buildable.build $ TL.intercalate ", " rs

renderUnit :: Unit -> Sim TL.Builder
renderUnit (Unit{..}) = pure $ displayShowB unitType


renderBuilding :: Building -> Sim TL.Builder
renderBuilding (Building{..}) = pure $ displayShowB buildingType

renderResource :: Resource -> Sim TL.Builder
renderResource (Resource{..}) = pure $ displayShowB resourceType

displayShowB :: (Show a) => a -> TL.Builder
displayShowB = F.Buildable.build . displayShowT

displayShowTL ::  Show a => a -> TL.Text
displayShowTL = utf8BuilderToLazyText . displayShow