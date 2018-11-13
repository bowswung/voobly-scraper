{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS -fno-warn-deprecations #-}
module Data.Mgz.Simulate.State where


import RIO
import Voobly.TH


import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict
import Data.Proxy(Proxy(..))
import qualified RIO.HashMap as HM
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified RIO.List as L

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.Events
import Data.Mgz.Utils


newtype MapTileIndex = MapTileIndex (Int, Int) deriving (Eq, Ord, Show)

mapTileCombinedIdx :: MapTile -> MapTileIndex
mapTileCombinedIdx MapTile{..} = MapTileIndex (mapTileX, mapTileY)

posToCombinedIdx :: Pos -> MapTileIndex
posToCombinedIdx Pos{..} = MapTileIndex (floor posX, floor posY)

newtype ReferencesObjectIdx = ReferencesObjectIdx ObjectId deriving (Eq, Ord, Show)
eventReferencesObjectIdx :: Event -> [ReferencesObjectIdx]
eventReferencesObjectIdx = map ReferencesObjectIdx . referencesObjectIds

newtype EventObjectIdAssignmentIdx = EventObjectIdAssignmentIdx{objectIdFromEventObjectIdAssignmentIdx ::  ObjectId} deriving (Eq, Ord, Show)
eventObjectIdAssignmentIdx :: Event -> [EventObjectIdAssignmentIdx]
eventObjectIdAssignmentIdx Event{..} =
  case eventType of
    EventTypeBuild b -> map EventObjectIdAssignmentIdx $ eventAssignObjectIds ++ catMaybes [fmap toObjectId $ eventBuildBuilding b]
    _ -> map EventObjectIdAssignmentIdx $ eventAssignObjectIds


newtype EventHighestObjectIdReferencedIdx = EventHighestObjectIdReferencedIdx ObjectId deriving (Eq, Ord, Show)
eventHighestObjectIdReferencedIdx :: Event -> Maybe EventHighestObjectIdReferencedIdx
eventHighestObjectIdReferencedIdx e =
  case L.reverse . L.sort $ eventReferencesObjectIdx e of
    (ReferencesObjectIdx o):_ -> Just . EventHighestObjectIdReferencedIdx $ o
    _ -> Nothing

-- this is basically the same as EventObjectIdAssignmentIdx at the moment
newtype EventBuildLinkedBuildingIdx = EventBuildLinkedBuildingIdx BuildingId deriving (Eq, Ord, Show)

eventBuildLinkedBuildingIdx :: Event -> Maybe EventBuildLinkedBuildingIdx
eventBuildLinkedBuildingIdx = fmap EventBuildLinkedBuildingIdx . eventLinkedBuilding


newtype EventTrainLinkedUnitIdx = EventTrainLinkedUnitIdx UnitId deriving (Eq, Ord, Show)

eventTrainLinkedUnitIdx :: Event -> Maybe EventTrainLinkedUnitIdx
eventTrainLinkedUnitIdx = fmap EventTrainLinkedUnitIdx . eventLinkedUnit


newtype ObjectPlacedByGameIdx = ObjectPlacedByGameIdx Bool deriving (Eq, Ord, Show)
objectPlacedByGameIdx :: Object -> ObjectPlacedByGameIdx
objectPlacedByGameIdx = ObjectPlacedByGameIdx . objectPlacedByGame


newtype ObjectHasBuildOrTrainEventIdx = ObjectHasBuildOrTrainEventIdx Bool deriving (Eq, Ord, Show)
objectHasBuildOrTrainEventIdx :: Object -> ObjectHasBuildOrTrainEventIdx
objectHasBuildOrTrainEventIdx o = ObjectHasBuildOrTrainEventIdx $ isJust (getBuildingPlaceEvent o) || isJust (getUnitTrainEvent o)


newtype ObjectWasDeletedIdx = ObjectWasDeletedIdx Bool deriving (Eq, Ord, Show)
objectWasDeletedIdx :: Object -> ObjectWasDeletedIdx
objectWasDeletedIdx = ObjectWasDeletedIdx . isJust . objectDeletedBy



newtype EventTrainedObjectTypeIdx = EventTrainedObjectTypeIdx ObjectType deriving (Eq, Ord, Show)

newtype EventTick = EventTick Int  deriving (Eq, Ord, Show)

eventTickIdx :: Event -> EventTick
eventTickIdx = EventTick . eventTick

eventTrainedObjectTypeIdx :: Event -> Maybe EventTrainedObjectTypeIdx
eventTrainedObjectTypeIdx = fmap EventTrainedObjectTypeIdx . eventTrainObjectType

newtype EventBuildObjectTypeIdx = EventBuildObjectTypeIdx ObjectType deriving (Eq, Ord, Show)
eventBuildObjectTypeIdx :: Event -> Maybe EventBuildObjectTypeIdx
eventBuildObjectTypeIdx = fmap EventBuildObjectTypeIdx . eventBuildBuildingObjectType

newtype EventSinglePosIdx = EventSinglePosIdx Pos deriving (Eq, Ord, Show)
eventSinglePosIdx :: Event -> Maybe EventSinglePosIdx
eventSinglePosIdx = fmap EventSinglePosIdx . getSingleEventPos



newtype EventResearchBuildingIdx = EventResearchBuildingIdx BuildingId deriving (Eq, Ord, Show)
eventResearchBuildingIdx :: Event -> Maybe EventResearchBuildingIdx
eventResearchBuildingIdx = fmap EventResearchBuildingIdx . eventResearchBuildingMaybe


eventActingObjectsIdx :: Event -> [ObjectId]
eventActingObjectsIdx = eventActingObjects

otRestrictWIdx :: Object -> OTRestrictW
otRestrictWIdx = otRestrictToOtRestrictW . getObjectRestrict

makeSimpleIxSet "ObjectSet" ''Object ['objectId
                                     , 'objectTypeW
                                     , 'objectPlacedByGameIdx
                                     , 'otRestrictToRestrictions
                                     , 'otRestrictWIdx
                                     , 'objectHasBuildOrTrainEventIdx
                                     , 'objectWasDeletedIdx
                                     , 'objectPlayer
                                     ]
makeSimpleIxSet "EventSet" ''Event ['eventId
                                   , 'eventTypeW
                                   , 'eventActingObjectsIdx
                                   , 'eventPlayerResponsible
                                   , 'eventReferencesObjectIdx
                                   , 'eventObjectIdAssignmentIdx
                                   , 'eventHighestObjectIdReferencedIdx
                                   , 'eventTrainedObjectTypeIdx
                                   , 'eventTickIdx
                                   , 'eventBuildLinkedBuildingIdx
                                   , 'eventTrainLinkedUnitIdx
                                   , 'eventBuildObjectTypeIdx
                                   , 'eventSinglePosIdx
                                   , 'eventResearchBuildingIdx
                                   ]
makeSimpleIxSet "MapTileSet" ''MapTile ['mapTileX, 'mapTileY, 'mapTileCombinedIdx]

data GameState = GameState {
  objects :: ObjectSet,
  events :: EventSet,
  mapTiles :: MapTileSet,
  playerInfos :: HM.HashMap PlayerId PlayerInfo
} deriving Show

emptyGameState :: Header -> GameState
emptyGameState h =
  let initialMap = IxSet.fromList $ map (\t -> MapTile (tilePositionX t) (tilePositionY t) []) (headerTiles h)
  in GameState IxSet.empty IxSet.empty initialMap (HM.fromList $ map (\i -> (playerInfoPlayerId i, i)) (headerPlayers h))

type Ticks = Int
type LastUsedIds = HM.HashMap PlayerId [ObjectId]
data SimState = SimState {
  ticks :: Ticks,
  gameState :: GameState,
  lastUsedIds :: LastUsedIds
}

type Sim a = StateT SimState (RIO LogFunc) a

getSelectedObjectIds :: EitherInheritOrIds -> PlayerId -> Sim [ObjectId]
getSelectedObjectIds (Left ()) pId = do
  m <- fmap lastUsedIds $ get
  case HM.lookup pId m of
    Nothing -> error $ "Expected to find inheritable ids for " ++ show pId ++ " but got nothing"
    Just xs -> pure xs
getSelectedObjectIds (Right xs) pId = modify' (\ss -> ss{lastUsedIds = HM.insert pId xs (lastUsedIds ss)}) >> pure xs

getObjectSet :: Sim ObjectSet
getObjectSet = fmap (objects . gameState) get

getEventSet :: Sim EventSet
getEventSet = fmap (events . gameState) get

getUnitsForPlayer :: [ObjectId] -> PlayerId -> Sim [ObjectUnit]
getUnitsForPlayer us i = mapM ((flip getUnitForPlayer) i) us

getObjectsForPlayer :: [ObjectId] -> PlayerId -> Sim [Object]
getObjectsForPlayer us i = mapM ((flip getObjectForPlayer) (Just i)) us


getUnit :: ObjectId -> Sim ObjectUnit
getUnit i = do
  o <- getObject i
  fmap asUnit $ updateWithRestriction o OTRestrictionIsUnit

updateWithRestriction :: Object -> OTRestriction -> Sim Object
updateWithRestriction o r = updateObject $ restrictObjectType o r

updateWithObjectType :: Object -> ObjectType -> Sim Object
updateWithObjectType o r = updateObject $ setObjectType o r

updateWithObjectTypes :: Object -> NonEmpty ObjectType -> Sim Object
updateWithObjectTypes o r = updateObject $ setObjectTypes o r

getUnitForPlayer :: ObjectId -> PlayerId -> Sim ObjectUnit
getUnitForPlayer i pId = do
  o <- getObjectForPlayer i (Just pId)
  fmap asUnit $ updateWithRestriction o OTRestrictionIsUnit



getObjectWithRestriction :: ObjectId -> OTRestriction -> Sim Object
getObjectWithRestriction i t = do
  o <- getObjectForPlayer i Nothing
  updateObject $ restrictObjectType o t

getObjectAsType :: ObjectId -> ObjectType -> Sim Object
getObjectAsType i t = do
  o <- getObjectForPlayer i Nothing
  updateObject $ setObjectType o t

getObject :: ObjectId -> Sim Object
getObject i = getObjectForPlayer i Nothing

getObjectForPlayer :: ObjectId -> Maybe PlayerId -> Sim Object
getObjectForPlayer i mpId = do
  mO <- lookupObject i
  case mO of
    Just o -> updateObjectWithMaybePlayerIfNone o mpId
    Nothing -> do
      updateObject $ newObject i mpId

getBuilding :: ObjectId -> Sim ObjectBuilding
getBuilding i = do
  o <- getObject i
  fmap asBuilding $ updateWithRestriction o OTRestrictionIsBuilding

getBuildingForPlayer :: ObjectId -> PlayerId -> Sim ObjectBuilding
getBuildingForPlayer i pId = do
  o <- getObjectForPlayer i (Just pId)
  fmap asBuilding $ updateWithRestriction o OTRestrictionIsBuilding

lookupObjects :: (ToObjectId a ) => [a] -> Sim [Object]
lookupObjects = mapM lookupObjectOrFail

lookupObject :: (ToObjectId a ) => a -> Sim (Maybe Object)
lookupObject i = do
  when (toObjectId i < ObjectId 0) $ error "GOT AN OBJECT ID < 0"
  ixset <- fmap (objects . gameState) $ get
  pure $ IxSet.getOne $ IxSet.getEQ (toObjectId i) ixset

lookupObjectOrFail :: (ToObjectId a ) => a -> Sim (Object)
lookupObjectOrFail i = do
  mo <- lookupObject i
  case mo of
    Just o -> pure o
    Nothing -> error $ "Could not find object with id " ++ (show $ toObjectId i)


lookupEventOrFail :: EventId -> Sim Event
lookupEventOrFail i = do
  s <- getEventSet
  case IxSet.getOne $ IxSet.getEQ i s of
    Just o -> pure o
    Nothing -> error $ "Could not find event with id " ++ (show $ i)



updateObject :: Object -> Sim Object
updateObject o = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{objects = IxSet.updateIx (objectId o) o (objects gs)}}
  pure o


addRealEvent :: Command -> Maybe PlayerId -> EventType -> Sim Event
addRealEvent c mP et = do
  ssP <- get
  let e = Event {
            eventId = EventId $ IxSet.size (events $ gameState ssP) + 1
          , eventTick = ticks ssP
          , eventKind = EventKindReal c
          , eventPlayerResponsible = mP
          , eventType = et
          , eventAssignObjectIds = []
          }
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{events = IxSet.insert e (events gs)}}
  pure e

addSimulatedEvent :: Int -> EventType -> Sim Event
addSimulatedEvent t et = do
  ssP <- get
  let e = Event {
            eventId = EventId $ IxSet.size (events $ gameState ssP) + 1
          , eventTick = t
          , eventKind = EventKindSimulated
          , eventPlayerResponsible = Just . PlayerId $ 0
          , eventType = et
          , eventAssignObjectIds = []
          }
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{events = IxSet.insert e (events gs)}}
  pure e



updateEvent :: Event -> Sim Event
updateEvent e = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{events = IxSet.updateIx (eventId e) e (events gs)}}
  pure e


findUnlinkedObjects :: Sim ObjectSet
findUnlinkedObjects = fmap (
  IxSet.getEQ (ObjectHasBuildOrTrainEventIdx False) .
  IxSet.getEQ (ObjectPlacedByGameIdx False)) $ getObjectSet

eventsPriorToObjectCreation :: (ToObjectId o) => o -> Sim EventSet
eventsPriorToObjectCreation o  = do
  eSet <- getEventSet

  let definitelyAfter = IxSet.getGTE (ReferencesObjectIdx $ toObjectId o) eSet
      firstEvent = headMaybe $ IxSet.toAscList (Proxy :: Proxy EventId) definitelyAfter
  case firstEvent of
    Nothing -> pure eSet
    Just e -> pure $ IxSet.getLT (eventId e) eSet

findEventsRangeForObjectCreation :: (ToObjectId o) => o  -> Maybe (NonEmpty ObjectType) -> Sim EventSet
findEventsRangeForObjectCreation o mOt  = do
  eventsPriorSet <- eventsPriorToObjectCreation o
  let restrictPreviousEvents = case mOt of
                                  -- these seem to be assigned an id when they are actually built?
                                 Just ot -> length (L.intersect [OT_TownCenter] (NE.toList ot) ) < 1
                                 Nothing -> True
  lastEventDefinitelyBefore <- if restrictPreviousEvents
    then do
      eventsThatWereDefinitelyBefore <- fmap (IxSet.getLT (EventObjectIdAssignmentIdx $ toObjectId o)) $ getEventSet
      pure $ headMaybe $ IxSet.toDescList (Proxy :: Proxy EventId) eventsThatWereDefinitelyBefore
    else pure Nothing
  pure $
    case lastEventDefinitelyBefore of
      Nothing -> eventsPriorSet
      Just e -> IxSet.getGT (eventId e) eventsPriorSet

findUnconsumedBuildOrWallEventsForObject :: Object -> Sim EventSet
findUnconsumedBuildOrWallEventsForObject o = do
  preEvents <- findEventsRangeForObjectCreation (objectId o) (getObjectTypes o)
  pure $
   maybe id (\ots -> ixsetGetIn (map (Just . EventBuildObjectTypeIdx) $ NE.toList ots)) (getObjectTypes o) .
   maybe id (IxSet.getEQ . Just) (objectPlayer o) .
   IxSet.getEQ (Nothing :: Maybe EventBuildLinkedBuildingIdx) .
   ixsetGetIn [EventTypeWBuild, EventTypeWWall] $
   preEvents


restrictToLastXSeconds :: Int -> EventSet -> EventSet
restrictToLastXSeconds s eSet =
  case headMaybe $ IxSet.toDescList (Proxy :: Proxy EventId) eSet of
    Nothing -> eSet
    Just e -> IxSet.getGT (EventTick $ eventTick e - (s * 1000)) eSet

findUnconsumedTrainEventsForObject:: Object -> Sim [Event]
findUnconsumedTrainEventsForObject o = do
  preEvents <- eventsPriorToObjectCreation (objectId o)
  let maybeTrainEvents =
       IxSet.toList .
       restrictToLastXSeconds (4 * 60) .
       maybe id (\ots -> ixsetGetIn (map (Just . EventTrainedObjectTypeIdx) $ NE.toList ots)) (getPossibleObjectTypes o) .
       maybe id (IxSet.getEQ . Just) (objectPlayer o) .
       IxSet.getEQ (Nothing :: Maybe EventTrainLinkedUnitIdx) .
       IxSet.getEQ EventTypeWTrain $
       preEvents

      uniqueBuildings = L.nub $ map eventTrainBuildingPartial maybeTrainEvents
  -- this is not very efficient - we need a better way of generalising about event timing effectively
  eventsRoundCreation <- findEventsRangeForObjectCreation o (getObjectTypes o)
  hmVals <-
    case headMaybe $ IxSet.toAscList (Proxy :: Proxy EventId) eventsRoundCreation of
      Just earliestEvent -> fmap catMaybes $ (flip mapM) uniqueBuildings $ \bid -> do
          recentRes <- mostRecentResearchEventAtBuildingBefore (eventId earliestEvent) bid
          case recentRes of
            Just ee -> pure . Just $ (bid, ee)
            Nothing -> pure Nothing
      Nothing -> pure []
  let hm = HM.fromList hmVals
  pure $ filter (not . shouldDropEvent hm) maybeTrainEvents

debugEventShort :: Event -> Sim ()
debugEventShort e = do
  traceShowM $ displayShowT (eventIdToInt . eventId $ e) <> "  " <> displayShowT (eventTypeW e)

shouldDropEvent :: HM.HashMap BuildingId EventId -> Event -> Bool
shouldDropEvent hm e =
  case HM.lookup (eventTrainBuildingPartial e) hm of
    Nothing -> False
    Just dropLowerThan -> (eventId e) < dropLowerThan

mostRecentResearchEventAtBuildingBefore :: EventId -> BuildingId -> Sim (Maybe EventId)
mostRecentResearchEventAtBuildingBefore eid bid = do
  mEvents <- fmap (
       IxSet.getEQ (Just . EventResearchBuildingIdx $ bid) .
       IxSet.getLT eid .
       IxSet.getEQ EventTypeWResearch)
       getEventSet
  case headMaybe $ IxSet.toDescList (Proxy :: Proxy EventId) mEvents of
    Just e -> pure . Just . eventId $ e
    Nothing -> pure Nothing


updateObjectWithMaybePlayerIfNone :: Object -> Maybe PlayerId -> Sim Object
updateObjectWithMaybePlayerIfNone o Nothing = pure o
updateObjectWithMaybePlayerIfNone o (Just pid) = updateObjectWithPlayerIfNone o pid

updateObjectWithPlayerIfNone :: Object -> PlayerId -> Sim Object
updateObjectWithPlayerIfNone o pid = do
  let no = setObjectPlayer o (Just pid)
  if no == o
    then pure o
    else updateObject no