{-# LANGUAGE TemplateHaskell    #-}

module Data.Mgz.Simulate.State where


import RIO
import Voobly.TH


import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict
import Data.Proxy(Proxy(..))
import qualified RIO.HashMap as HM
import Data.List.NonEmpty(NonEmpty(..))

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

newtype EventObjectIdAssignmentIdx = EventObjectIdAssignmentIdx ObjectId deriving (Eq, Ord, Show)
eventObjectIdAssignmentIdx :: Event -> [EventObjectIdAssignmentIdx]
eventObjectIdAssignmentIdx Event{..} =
  case eventType of
    EventTypeBuild b -> map EventObjectIdAssignmentIdx $   catMaybes [fmap toObjectId $ eventBuildBuilding b]
    _ -> []


newtype ObjectPlacedByGameIdx = ObjectPlacedByGameIdx Bool deriving (Eq, Ord, Show)
objectPlacedByGameIdx :: Object -> ObjectPlacedByGameIdx
objectPlacedByGameIdx = ObjectPlacedByGameIdx . objectPlacedByGame



eventActingObjectsIdx :: Event -> [ObjectId]
eventActingObjectsIdx = eventActingObjects




makeSimpleIxSet "ObjectSet" ''Object ['objectId, 'objectTypeW, 'objectPlacedByGameIdx, 'otRestrictToRestrictions]
makeSimpleIxSet "EventSet" ''Event ['eventId, 'eventTypeW, 'eventActingObjectsIdx, 'eventPlayerResponsible, 'eventReferencesObjectIdx, 'eventObjectIdAssignmentIdx]
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

type Sim a = State SimState a

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


updateObject :: Object -> Sim Object
updateObject o = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{objects = IxSet.updateIx (objectId o) o (objects gs)}}
  pure o


addRealEvent :: Command -> Maybe PlayerId -> EventType -> Sim ()
addRealEvent c mP et = modify' $ \ss ->
  let gs = gameState ss
      e = Event {
            eventId = EventId $ IxSet.size (events gs) + 1
          , eventTick = ticks ss
          , eventKind = EventKindReal c
          , eventPlayerResponsible = mP
          , eventType = et
          }
  in ss{gameState = gs{events = IxSet.insert e (events gs)}}


updateEvent :: Event -> Sim Event
updateEvent e = do
  modify' $ \ss ->
    let gs = gameState ss
    in ss{gameState = gs{events = IxSet.updateIx (eventId e) e (events gs)}}
  pure e




findEventsRangeForObjectCreation :: ObjectId -> Maybe ObjectType -> Sim EventSet
findEventsRangeForObjectCreation oid mOt  = do
  eSet <- getEventSet
  let restrictPreviousEvents = not $  mOt `elem` [Just OT_TownCenter]  -- town centers seem to be assigned an id when they are actually built?
  let definitelyAfter = IxSet.getGTE (ReferencesObjectIdx oid) eSet
      firstEvent = headMaybe $ IxSet.toAscList (Proxy :: Proxy EventId) definitelyAfter
      definitelyBefore = IxSet.getLT (EventObjectIdAssignmentIdx oid) eSet
      lastEvent = if restrictPreviousEvents then headMaybe $ IxSet.toDescList (Proxy :: Proxy EventId) definitelyBefore else Nothing
  let finalSet =
        case firstEvent of
          Nothing -> eSet
          Just e -> IxSet.getLT (eventId e) eSet
  pure $
    case lastEvent of
      Nothing -> finalSet
      Just e -> IxSet.getGT (eventId e) finalSet


updateObjectWithMaybePlayerIfNone :: Object -> Maybe PlayerId -> Sim Object
updateObjectWithMaybePlayerIfNone o Nothing = pure o
updateObjectWithMaybePlayerIfNone o (Just pid) = updateObjectWithPlayerIfNone o pid

updateObjectWithPlayerIfNone :: Object -> PlayerId -> Sim Object
updateObjectWithPlayerIfNone o pid =
  case objectPlayer o of
    Nothing -> updateObject $ setObjectPlayer o (Just pid)
    Just _ -> pure o