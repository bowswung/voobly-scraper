{-# LANGUAGE TemplateHaskell    #-}

module Data.Mgz.Simulate where

import RIO
import Voobly.TH

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict



newtype ObjectId = ObjectId Int deriving (Show, Eq, Ord) -- object id from rec file - we don't know anything about it!

newtype ObjectUnit = ObjectUnit Object -- just for the types
newtype ObjectBuilding = ObjectBuilding Object -- just for the types

newtype UnitId = UnitId ObjectId deriving (Show, Eq, Ord)
newtype BuildingId = BuildingId ObjectId deriving (Show, Eq, Ord)
newtype ResourceId = ResourceId ObjectId deriving (Show, Eq, Ord)

newtype PlayerId = PlayerId Int deriving (Show, Eq, Ord) -- 1,2 etc - same as the rec file
newtype EventId = EventId Int deriving (Show, Eq, Ord) -- sequence


asUnit :: Object -> ObjectUnit
asUnit o =
  if objectTypeW o == ObjectTypeWUnit
    then ObjectUnit o
    else error "Could not get object asUnit"

unitId :: ObjectUnit -> UnitId
unitId (ObjectUnit o) = UnitId . objectId $ o


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


data BuildingType =
    BuildingTypeUnknown
  | BuildingTypeKnown ObjectType
  | BuildingTypeOneOf (NonEmpty ObjectType)
  deriving (Show, Eq, Ord)

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
  eventPrimaryUnits :: [UnitId],
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
  eventTargetedMilitaryOrderTarget :: Int -- this should be unitId maybe? can you guard buildings?
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
  deriving (Show, Eq, Ord)

data Unit = Unit {
  unitType :: UnitType
} deriving (Show, Eq, Ord)

data Building = Building {
  buildingType :: BuildingType
} deriving (Show, Eq, Ord)

data ResourceType = ResourceTypeUnknown
  deriving (Show, Eq, Ord)

data Resource = Resource {
  resourceType :: ResourceType
} deriving (Show, Eq, Ord)

data ObjectInfo =
    ObjectInfoUnit Unit
  | ObjectInfoBuilding Building
  | ObjectInfoResource Resource
  | ObjectInfoUnknown
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
    ObjectInfoUnknown -> ObjectTypeWUnknown

data Object = Object {
  objectId :: ObjectId,
  objectPlayer :: Maybe PlayerId,
  objectInfo :: ObjectInfo
} deriving (Show, Eq, Ord)




makeSimpleIxSet "ObjectSet" ''Object ['objectId]
makeSimpleIxSet "EventSet" ''Event ['eventId]


data GameState = GameState {
  objects :: ObjectSet,
  events :: EventSet
} deriving Show

simulate :: HasLogFunc env => RecInfo -> RIO env GameState
simulate RecInfo{..} = do
  logInfo "Start simulating"
  logInfo "Building histories"
  let initialGS = GameState IxSet.empty IxSet.empty

  let sWithHistories = snd $ execState (mapM addToHistory recInfoOps) (0, initialGS)
  logInfo $ "Total events: " <> displayShow (IxSet.size . events $ sWithHistories )
  --logInfo $ "Found units: " <> displayShow (IxSet.size . units $ sWithHistories )
  --logInfo $ "Found buildings: " <> displayShow (IxSet.size . buildings $ sWithHistories )



  pure $ error "Nothing"

type Ticks = Int
type Sim a = State (Ticks, GameState) a

addToHistory :: Op -> Sim ()
addToHistory (OpTypeSync OpSync{..}) = modify' (\(t,s)-> (t + opSyncTime, s))
addToHistory (OpTypeCommand cmd) = addCommandToHistory cmd
addToHistory _ = pure ()


addCommandToHistory :: Command -> State (Ticks, GameState) ()
addCommandToHistory c@(CommandTypePrimary CommandPrimary{..}) = do
  target <- getObject commandPrimaryTargetId
  units <- getUnitsForPlayer commandPrimaryUnitIds commandPrimaryPlayerId
  eType <- pure $ EventTypePrimary $ EventPrimary {
      eventPrimaryUnits = map unitId units
    , eventPrimaryTarget = objectId target
    , eventPrimaryPos = commandPrimaryPos
    }
  addRealEvent c (Just commandPrimaryPlayerId) eType

addCommandToHistory _ = pure ()



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
                      | otherwise -> error "PlayerId CHANGED??? "

    Nothing -> do
      let o = Object {
                objectId = ObjectId i
              , objectPlayer = fmap PlayerId mpId
              , objectInfo = ObjectInfoUnknown
              }
      updateObject o

getUnitsForPlayer :: [Int] -> Int -> Sim [ObjectUnit]
getUnitsForPlayer us i = mapM ((flip getUnitForPlayer) i) us

getUnit :: Int -> Sim ObjectUnit
getUnit i = do
  o <- getObject i
  fmap asUnit $ convertObj o ObjectTypeWUnit

getUnitForPlayer :: Int -> Int -> Sim ObjectUnit
getUnitForPlayer i pId = do
  o <- getObjectForPlayer i (Just pId)
  fmap asUnit $ convertObj o ObjectTypeWUnit

convertObj :: Object -> ObjectTypeW -> Sim Object
convertObj o w =
  case objectTypeW o of
    ObjectTypeWUnknown -> do
      let oInfo = case w of
            ObjectTypeWUnit -> ObjectInfoUnit $ Unit UnitTypeUnknown
            ObjectTypeWBuilding -> ObjectInfoBuilding $ Building BuildingTypeUnknown
            ObjectTypeWResource -> ObjectInfoResource $ Resource ResourceTypeUnknown
            ObjectTypeWUnknown -> objectInfo o
          newO = o{objectInfo = oInfo}
      updateObject newO
    ow | ow == w -> pure o
       | otherwise -> error "Could not coerce"

lookupObject :: ObjectId -> Sim (Maybe Object)
lookupObject i = do
  ixset <- fmap (objects . snd) $ get
  pure $ IxSet.getOne $ IxSet.getEQ i ixset

updateObject :: Object -> Sim Object
updateObject o = modify' (\(t, gs) ->
  (t, gs{objects = IxSet.insert o (objects gs)})
  ) >> pure o


addRealEvent :: Command -> Maybe Int -> EventType -> Sim ()
addRealEvent c mP et = modify' $ \(t, gs) ->
  let e = Event {
            eventId = EventId $ IxSet.size (events gs) + 1
          , eventTick = t
          , eventKind = EventKindReal c
          , eventPlayerResponsible = fmap PlayerId mP
          , eventType = et
          }
  in (t, gs{events = IxSet.insert e (events gs)})






replay :: HasLogFunc env => GameState -> RIO env ()
replay _ =
  logInfo "Game start"
