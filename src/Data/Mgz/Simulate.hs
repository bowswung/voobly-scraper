{-# LANGUAGE TemplateHaskell    #-}

module Data.Mgz.Simulate where

import RIO
import Voobly.TH

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict
import Data.Proxy(Proxy(..))
import qualified Data.Text.Format as F
import qualified Data.Text.Buildable as F.Buildable
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
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


data BuildingType =
    BuildingTypeUnknown
  | BuildingTypeKnown ObjectType
  | BuildingTypeOneOf (NonEmpty ObjectType)
  deriving (Show, Eq, Ord)


getBuildingType :: Int -> BuildingType
getBuildingType i = BuildingTypeKnown $ normaliseObjectType i

isVillagerType :: ObjectType -> Bool
isVillagerType OT_Villager = True
isVillagerType _ = False

isNotVillagerOrMilitary :: ObjectType -> Bool
isNotVillagerOrMilitary OT_Sheep = True
isNotVillagerOrMilitary _ = False

getUnitType :: Int -> UnitType
getUnitType i =
  let t = normaliseObjectType i
  in if isVillagerType t
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
  buildingType :: BuildingType,
  buildingPos :: Maybe Pos
} deriving (Show, Eq, Ord)

data ResourceType = ResourceTypeUnknown
  deriving (Show, Eq, Ord)

data Resource = Resource {
  resourceType :: ResourceType,
  resourcePos :: Maybe Pos
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
  logInfo "Building base events"
  let initialGS = GameState IxSet.empty IxSet.empty

  let sBasic = gameState $ execState (mapM buildBasicEvents recInfoOps) (SimState 0 initialGS [])
  logInfo $ "Total events: " <> displayShow (IxSet.size . events $ sBasic)

  logInfo "Making simple inferences"
  let sWithSimpleInferences = gameState $ execState makeSimpleInferences (SimState 0 sBasic [])



  pure $ sWithSimpleInferences





type Ticks = Int
type LastUsedIds = [Int]
data SimState = SimState {
  ticks :: Ticks,
  gameState :: GameState,
  lastUsedIds :: LastUsedIds
}

type Sim a = State SimState a

makeSimpleInferences :: Sim ()
makeSimpleInferences = do
  pure ()


buildBasicEvents :: Op -> Sim ()
buildBasicEvents (OpTypeSync OpSync{..}) = modify' (\ss -> ss{ticks = ticks ss + opSyncTime})
buildBasicEvents (OpTypeCommand cmd) = addCommandAsEvent cmd
buildBasicEvents _ = pure ()


addCommandAsEvent :: Command -> Sim ()
addCommandAsEvent c@(CommandTypePrimary CommandPrimary{..}) = do
  target <- getObject commandPrimaryTargetId
  uids <- getUnitIds commandPrimaryUnitIds

  objs <- getObjectsForPlayer uids commandPrimaryPlayerId
  let eType = EventTypePrimary $ EventPrimary {
      eventPrimaryObjects = map objectId objs
    , eventPrimaryTarget = objectId target
    , eventPrimaryPos = commandPrimaryPos
    }
  addRealEvent c (Just commandPrimaryPlayerId) eType

addCommandAsEvent c@(CommandTypeMove CommandMove{..}) = do
  uids <- getUnitIds commandMoveUnitIds
  units <- getUnitsForPlayer uids commandMovePlayerId
  let eType = EventTypeMove $ EventMove {
      eventMoveUnits = map unitId units
    , eventMovePos = commandMovePos
    }
  addRealEvent c (Just commandMovePlayerId) eType

addCommandAsEvent c@(CommandTypeStance CommandStance{..}) = do
  uids <- getUnitIds commandStanceUnitIds
  units <- mapM getUnit uids
  let eType = EventTypeMilitaryDisposition $ EventMilitaryDisposition {
      eventMilitaryDispositionUnits = map unitId units
    , eventMilitaryDispositionType = MilitaryDispositionStance commandStanceStance
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeFormation CommandFormation{..}) = do
  uids <- getUnitIds commandFormationUnitIds
  units <- getUnitsForPlayer uids commandFormationPlayerId

  let eType = EventTypeMilitaryDisposition $ EventMilitaryDisposition {
      eventMilitaryDispositionUnits = map unitId units
    , eventMilitaryDispositionType = MilitaryDispositionFormation commandFormationFormation
    }
  addRealEvent c (Just commandFormationPlayerId) eType

addCommandAsEvent c@(CommandTypeGuard CommandGuard{..}) = do
  target <- getObject commandGuardGuarded
  uids <- getUnitIds commandGuardUnitIds
  units <- mapM getUnit uids

  let eType = EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
      eventTargetedMilitaryOrderUnits = map unitId units
    , eventTargetedMilitaryOrderType = TargetedMilitaryOrderGuard
    , eventTargetedMilitaryOrderTarget = objectId target
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeFollow CommandFollow{..}) = do
  target <- getObject commandFollowFollowed
  uids <- getUnitIds commandFollowUnitIds
  units <- mapM getUnit uids

  let eType = EventTypeTargetedMilitaryOrder $ EventTargetedMilitaryOrder {
      eventTargetedMilitaryOrderUnits = map unitId units
    , eventTargetedMilitaryOrderType = TargetedMilitaryOrderFollow
    , eventTargetedMilitaryOrderTarget = objectId target
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypePatrol CommandPatrol{..}) = do
  uids <- getUnitIds commandPatrolUnitIds
  units <- mapM getUnit uids

  let eType = EventTypePatrol $ EventPatrol {
      eventPatrolUnits = map unitId units
    , eventPatrolWaypoints = commandPatrolWaypoints
    }
  addRealEvent c Nothing eType

addCommandAsEvent c@(CommandTypeBuild CommandBuild{..}) = do
  uids <- getUnitIds commandBuildBuilders
  units <- getUnitsForPlayer uids commandBuildPlayerId

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
    , eventTrainType = getUnitType commandTrainUnitType
    , eventTrainNumber = commandTrainNumber
    }
  addRealEvent c Nothing eType

addCommandAsEvent _ = pure ()

getUnitIds :: [Int] -> Sim [Int]
getUnitIds [] = do
  ss <- get
  pure $ lastUsedIds ss
getUnitIds xs = modify' (\ss -> ss{lastUsedIds = xs}) >> pure xs

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
                      | otherwise -> pure o --error "PlayerId CHANGED??? "

    Nothing -> do
      let o = Object {
                objectId = ObjectId i
              , objectPlayer = fmap PlayerId mpId
              , objectInfo = ObjectInfoUnknown
              }
      updateObject o

getUnitsForPlayer :: [Int] -> Int -> Sim [ObjectUnit]
getUnitsForPlayer us i = mapM ((flip getUnitForPlayer) i) us

getObjectsForPlayer :: [Int] -> Int -> Sim [Object]
getObjectsForPlayer us i = mapM ((flip getObjectForPlayer) (Just i)) us


getUnit :: Int -> Sim ObjectUnit
getUnit i = do
  o <- getObject i
  fmap asUnit $ convertObj o ObjectTypeWUnit

getUnitForPlayer :: Int -> Int -> Sim ObjectUnit
getUnitForPlayer i pId = do
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
            ObjectTypeWResource -> ObjectInfoResource $ Resource ResourceTypeUnknown Nothing
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

  let r = evalState renderEvents (SimState 0 gs [])


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
                ObjectInfoUnknown -> pure $ "Unknown object"
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
            ObjectInfoUnknown -> pure $ "Unknown"
      p <- renderPlayer objectPlayer
      pure $ p <> " " <> t

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