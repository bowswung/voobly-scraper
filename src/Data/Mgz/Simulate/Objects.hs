module Data.Mgz.Simulate.Objects(
  EventId(..),
  MapTile(..),
  unitId,
  asUnit,
  ObjectUnit,
  ObjectBuilding,
  objectFromObjectBuilding,
  UnitId,
  BuildingId,
  buildingId,
  asBuilding,
  Object,
  newObject,
  OTRestriction(..),
  OTRestrict(..),
  OTRestrictW(..),
  otRestrictToOtRestrictW,
  setObjectType,
  getObjectType,
  getObjectTypes,
  getBuildingPlaceEvent,
  setBuildingPlaceEvent,
  setUnitTrainEvent,
  getUnitTrainEvent,
  setObjectTypes,
  restrictObjectType,
  --exactlyMeetsRestriction,
  anyMeetsRestriction,
  otRestrictToRestrictions,
 -- HasObjectRestrict(..),
  objectFromObjectRaw,
  MapObject(..),
  getObjectRestrict,
  ToObjectId(..),
  objectId,
  objectPlayer,
  objectInfo,
  objectPlacedByGame,
  objectPosHistory,
  addObjectPos,
  setObjectPlayer,
  objectTypeW,
  isVillager,
  isHerdable,
  isResource,
  isObjectEnemy,
  isMilitaryUnit,
  isUnit,
  isMonk,
  isObjectFriend,
  isBuilding,
  isNotDropoffBuilding
  ) where

import RIO


import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified RIO.List as L


import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Utils

newtype EventId = EventId {eventIdToInt :: Int} deriving (Show, Eq, Ord) -- sequence

newtype ObjectUnit = ObjectUnit Object -- just for the types
newtype UnitId = UnitId ObjectId deriving (Show, Eq, Ord)

unitId :: ObjectUnit -> UnitId
unitId (ObjectUnit o) = UnitId . objectId $ o

asUnit :: Object -> ObjectUnit
asUnit o =
  if objectTypeW o == ObjectTypeWUnit
    then ObjectUnit o
    else error $ "Could not get object asUnit" ++ show o


newtype ObjectBuilding = ObjectBuilding{_objectFromObjectBuilding :: Object} -- just for the types
objectFromObjectBuilding :: ObjectBuilding -> Object
objectFromObjectBuilding = _objectFromObjectBuilding

newtype BuildingId = BuildingId ObjectId deriving (Show, Eq, Ord)
buildingId :: ObjectBuilding -> BuildingId
buildingId (ObjectBuilding o) = BuildingId . objectId $ o

asBuilding :: Object -> ObjectBuilding
asBuilding o =
  if objectTypeW o == ObjectTypeWBuilding
    then ObjectBuilding o
    else error $ "Could not get object asBuilding" ++ show o



data Object = Object {
  _objectId :: ObjectId,
  _objectPlayer :: Maybe PlayerId,
  _objectInfo :: ObjectInfo,
  _objectPlacedByGame :: Bool,
  _objectPosHistory :: [Pos]
} deriving (Show, Eq, Ord)


objectId :: Object -> ObjectId
objectId = _objectId
objectPlayer :: Object -> Maybe PlayerId
objectPlayer = _objectPlayer

objectPosHistory :: Object -> [Pos]
objectPosHistory = _objectPosHistory

addObjectPos :: Object -> Pos -> Object
addObjectPos o p = o{_objectPosHistory = objectPosHistory o ++ [p]}

setObjectPlayer :: Object -> Maybe PlayerId -> Object
setObjectPlayer o m =
  case (objectPlayer o, m) of
    (Nothing, Nothing) -> o
    (Nothing, Just _) -> o{_objectPlayer = m}
    (Just _, Nothing) -> o -- ignore this for now
    (Just old, Just new) | old == new -> o
                         | isPlayerGaia old -> o  --sheep stealing
                         | otherwise -> error $ "Cannot change pid from " ++ show old ++ " to " ++ show new ++ " in object "++ show o

objectInfo :: Object -> ObjectInfo
objectInfo = _objectInfo
objectPlacedByGame :: Object -> Bool
objectPlacedByGame = _objectPlacedByGame

newObject :: ObjectId -> Maybe PlayerId -> Object
newObject i p = Object {
    _objectId = i
  , _objectPlayer = p
  , _objectInfo = ObjectInfoUnknown OTRestrictNone
  , _objectPlacedByGame = False
  , _objectPosHistory = []
  }

objectFromObjectRaw :: ObjectRaw -> (Object, MapObject)
objectFromObjectRaw oRaw@ObjectRaw{..} =
  let bo = newObject objectRawObjectId (Just objectRawOwner)
      o = bo{_objectPlacedByGame = True, _objectPosHistory = [objectRawPos]}
      mo = MapObject (OTRestrictKnown objectRawUnitId) objectRawOwner oRaw
  in (case objectRawType of
       70 -> o {_objectInfo = ObjectInfoUnit $ Unit (OTRestrictKnown objectRawUnitId) Nothing}
       80 -> o {_objectInfo = ObjectInfoBuilding $ Building (OTRestrictKnown objectRawUnitId) (Just objectRawPos) Nothing}
       _ -> o {_objectInfo = ObjectInfoMapObject mo},
       mo
       )

data OTRestrict =
    OTRestrictKnown ObjectType
  | OTRestrictOneOf (NonEmpty ObjectType)
  | OTRestrictGeneral (NonEmpty OTRestriction)
  | OTRestrictNone
  deriving (Show, Eq, Ord)

-- this is used for inddexing
data OTRestrictW =
    OTRestrictWKnown
  | OTRestrictWOneOf
  | OTRestrictWGeneral
  | OTRestrictWNone
  deriving (Show, Eq, Ord)

otRestrictToOtRestrictW :: OTRestrict -> OTRestrictW
otRestrictToOtRestrictW (OTRestrictKnown _) = OTRestrictWKnown
otRestrictToOtRestrictW (OTRestrictOneOf _) = OTRestrictWOneOf
otRestrictToOtRestrictW (OTRestrictGeneral _) = OTRestrictWGeneral
otRestrictToOtRestrictW (OTRestrictNone) = OTRestrictWNone

objectTypeW :: Object -> ObjectTypeW
objectTypeW o =
  case objectInfo o of
    ObjectInfoUnit _ -> ObjectTypeWUnit
    ObjectInfoBuilding _ -> ObjectTypeWBuilding
    ObjectInfoMapObject _ -> ObjectTypeWMapObject
    ObjectInfoUnknown _ -> ObjectTypeWUnknown


data ObjectInfo =
    ObjectInfoUnit Unit
  | ObjectInfoBuilding Building
  | ObjectInfoMapObject MapObject
  | ObjectInfoUnknown OTRestrict
  deriving (Show, Eq, Ord)

data Unit = Unit {
  unitType :: OTRestrict,
  unitTrainEvent :: Maybe EventId
} deriving (Show, Eq, Ord)

newUnit :: Unit
newUnit = Unit {
    unitType = OTRestrictGeneral $ singleNonEmpty OTRestrictionIsUnit
  , unitTrainEvent = Nothing
  }

data Building = Building {
  buildingType :: OTRestrict,
  buildingPos :: Maybe Pos,
  buildingPlaceEvent :: Maybe EventId
} deriving (Show, Eq, Ord)

newBuilding :: Building
newBuilding = Building {
    buildingType = OTRestrictGeneral $ singleNonEmpty OTRestrictionIsBuilding
  , buildingPos = Nothing
  , buildingPlaceEvent = Nothing
  }



getBuildingPlaceEvent :: Object -> Maybe (EventId)
getBuildingPlaceEvent = buildingPlaceEvent . extractBuilding

setBuildingPlaceEvent :: Object -> Maybe (EventId) -> Object
setBuildingPlaceEvent o e =
  let b = extractBuilding o
      newB = b{buildingPlaceEvent = e}
  in o{_objectInfo = ObjectInfoBuilding newB}


extractBuilding :: Object -> Building
extractBuilding Object{..} =
  case _objectInfo of
    ObjectInfoBuilding b -> b
    _ -> error "Could not extract building"

getUnitTrainEvent :: Object -> Maybe (EventId)
getUnitTrainEvent = unitTrainEvent . extractUnit

setUnitTrainEvent :: Object -> Maybe (EventId) -> Object
setUnitTrainEvent o e =
  let b = extractUnit o
      newB = b{unitTrainEvent = e}
  in o{_objectInfo = ObjectInfoUnit newB}


extractUnit :: Object -> Unit
extractUnit Object{..} =
  case _objectInfo of
    ObjectInfoUnit b -> b
    _ -> error "Could not extract unit"

data MapObject = MapObject {
  mapObjectType :: OTRestrict,
  mapObjectOwner :: PlayerId,
  mapObjectOriginal :: ObjectRaw
} deriving (Show, Eq, Ord)


data MapTile = MapTile {
  mapTileX :: Int,
  mapTileY :: Int,
  mapTileObjects :: [MapObject]
} deriving (Show, Eq, Ord)


class ToObjectId a where
  toObjectId :: a -> ObjectId

instance ToObjectId ObjectId where
  toObjectId = id

instance ToObjectId Object where
  toObjectId = objectId

instance ToObjectId UnitId where
  toObjectId (UnitId i) = i

instance ToObjectId BuildingId where
  toObjectId (BuildingId i) = i


class HasObjectRestrict a where
  getObjectRestrict :: a -> OTRestrict
  setObjectRestrict :: a -> OTRestrict -> a

instance HasObjectRestrict Object where
  getObjectRestrict o =
    case objectInfo o of
      ObjectInfoUnit u -> getObjectRestrict u
      ObjectInfoBuilding u -> getObjectRestrict u
      ObjectInfoMapObject u -> getObjectRestrict u
      ObjectInfoUnknown u -> u
  setObjectRestrict o t =
    let ni =
          case objectInfo o of
            ObjectInfoUnit u -> ObjectInfoUnit $ setObjectRestrict u t
            ObjectInfoBuilding u -> ObjectInfoBuilding $ setObjectRestrict u t
            ObjectInfoMapObject u -> ObjectInfoMapObject $ setObjectRestrict u t
            ObjectInfoUnknown old ->
              let new = setObjectRestrict old t
              in
                case otRestrictToObjectTypeW new of
                  ObjectTypeWUnit -> ObjectInfoUnit newUnit{unitType = new}
                  ObjectTypeWBuilding -> ObjectInfoBuilding newBuilding{buildingType = new}
                  ObjectTypeWMapObject -> error "Should not be possible to set a map object type on a unknown object"
                  ObjectTypeWUnknown -> ObjectInfoUnknown new
    in o{_objectInfo = ni}

instance HasObjectRestrict Unit where
  getObjectRestrict = unitType
  setObjectRestrict u t = u{unitType = setObjectRestrict (unitType u) t}

instance HasObjectRestrict Building where
  getObjectRestrict = buildingType
  setObjectRestrict u t = u{buildingType = setObjectRestrict (buildingType u) t}

instance HasObjectRestrict MapObject where
  getObjectRestrict = mapObjectType
  setObjectRestrict u t = u{mapObjectType = setObjectRestrict (mapObjectType u) t}


instance HasObjectRestrict OTRestrict where
  getObjectRestrict = id
  setObjectRestrict OTRestrictNone OTRestrictNone = OTRestrictNone
  setObjectRestrict old OTRestrictNone = error $ "Attempted to downgrade object from known " ++ show old ++ " to None"

  setObjectRestrict old (OTRestrictGeneral xs) = foldl' applyRestriction old xs
  setObjectRestrict (OTRestrictKnown old) (OTRestrictKnown new) =
    if old == new
      then OTRestrictKnown new
      else error $ "Tried to change the known type of an object from " ++ show old ++ " to " ++ show new
  setObjectRestrict (OTRestrictKnown old) new = error $ "Attempted to downgrade object from known " ++ show old ++ " to " ++ show new

  setObjectRestrict OTRestrictNone new = new

  setObjectRestrict (OTRestrictOneOf old) (OTRestrictKnown new) =
    if new `elemNonEmpty` old
      then OTRestrictKnown new
      else error $ "Tried to set the known type of an object from " ++ show old ++ " to " ++ show new
  setObjectRestrict (OTRestrictOneOf old) (OTRestrictOneOf new) =
    case L.nub $  L.intersect (NE.toList old) (NE.toList new) of
      [] -> error $ "Tried to set the oneof type of an object to  " ++ show new ++ " but there was no overlap with old " ++ show old
      [a] -> OTRestrictKnown a
      as -> OTRestrictOneOf $ nonEmptyPartial as

 -- should we do some checking here?
  setObjectRestrict (OTRestrictGeneral _) new = new



setObjectType :: Object -> ObjectType -> Object
setObjectType o t = setObjectRestrict o (OTRestrictKnown t)

setObjectTypes :: Object -> NonEmpty ObjectType -> Object
setObjectTypes o t = setObjectRestrict o (OTRestrictOneOf t)

restrictObjectType :: Object -> OTRestriction -> Object
restrictObjectType o r = setObjectRestrict o (OTRestrictGeneral $ singleNonEmpty r)

applyRestriction :: OTRestrict -> OTRestriction -> OTRestrict
applyRestriction orig@(OTRestrictKnown t) r =
  if anyMeetsRestriction orig r
    then orig
    else error $ "Restriction violated when apply restriction " ++ show r ++ " to known object " ++ show t
applyRestriction (OTRestrictOneOf ots) r =
  case L.nub $ NE.filter ((flip anyMeetsRestriction) r . OTRestrictKnown) ots of
    [] -> error $ "No possible object types found when applying restriction " ++ show r ++ " to list of objects " ++ show ots
    [a] -> OTRestrictKnown a
    as -> OTRestrictOneOf $ nonEmptyPartial as
applyRestriction orig@(OTRestrictGeneral rs) r =
  if r `elemNonEmpty` rs -- we already have this restriction
    then orig
    else otRestrictFromRestictions (NE.nub $ NE.cons r rs)
applyRestriction (OTRestrictNone) r = otRestrictFromRestictions $ singleNonEmpty r

otRestrictFromRestictions :: NonEmpty OTRestriction -> OTRestrict
otRestrictFromRestictions restrictions =
  case L.nub $ objectTypesForRestrictions restrictions of
     [] -> error $ "No possible object types found when applying restrictions " ++ show restrictions
     [a] -> OTRestrictKnown a
     as ->
       if length as < 10
        then OTRestrictOneOf $ nonEmptyPartial as
        else OTRestrictGeneral restrictions

anyMeetsRestriction :: (HasObjectRestrict a) => a -> OTRestriction -> Bool
anyMeetsRestriction a r =
  case getObjectRestrict a of
    OTRestrictKnown t -> hmMatches t r objectTypeToRestrictionMap
    OTRestrictOneOf ts -> or . NE.toList $ NE.map (\t -> hmMatches t r objectTypeToRestrictionMap) ts
    OTRestrictGeneral rs -> elemNonEmpty r rs
    OTRestrictNone -> False
{-
exactlyMeetsRestriction :: (HasObjectRestrict a) => a -> OTRestriction -> Bool
exactlyMeetsRestriction a r =
  case getObjectRestrict a of
    OTRestrictKnown t -> HM.lookup t objectTypeToRestrictionMap == Just [r]
    OTRestrictOneOf ts -> and . NE.toList $ NE.map (\t -> exactlyMeetsRestriction (OTRestrictKnown t) r) ts
    OTRestrictGeneral rs -> NE.head rs == r
    OTRestrictNone -> False-}

getObjectType :: (HasObjectRestrict a) => a -> Maybe ObjectType
getObjectType o =
  case getObjectRestrict o of
    OTRestrictKnown ot -> Just ot
    _ -> Nothing

getObjectTypes :: (HasObjectRestrict a) => a -> Maybe (NonEmpty ObjectType)
getObjectTypes o =
  case getObjectRestrict o of
    OTRestrictKnown ot -> Just $ nonEmptyPartial [ot]
    OTRestrictOneOf ot -> Just ot
    _ -> Nothing



otRestrictToRestrictions :: (HasObjectRestrict a) => a -> [OTRestriction]
otRestrictToRestrictions a =
  case getObjectRestrict a of
    OTRestrictKnown t -> lookupObjectTypeRestrictions t
    OTRestrictOneOf ts -> L.nub . concat $ map lookupObjectTypeRestrictions $ NE.toList ts
    OTRestrictGeneral rs -> NE.toList rs
    OTRestrictNone -> []


otRestrictToObjectTypeW :: OTRestrict -> ObjectTypeW
otRestrictToObjectTypeW (OTRestrictKnown ot) = lookupObjectTypeW ot
otRestrictToObjectTypeW (OTRestrictOneOf ots) =
  case NE.nub $ NE.map lookupObjectTypeW ots of
    (w:|[]) -> w
    _ -> ObjectTypeWUnknown

otRestrictToObjectTypeW (OTRestrictGeneral rs) =
  case L.nub . catMaybes $ map restrictionToKnownObjectTypeW (NE.toList rs) of
    [] -> ObjectTypeWUnknown
    [a] -> a
    as -> error $ "Inconsistent restrictions " ++ show rs ++ " resulted in impossible combination of objecttypews " ++ show as


otRestrictToObjectTypeW (OTRestrictNone) = ObjectTypeWUnknown






-- @teams
isObjectEnemy :: Maybe PlayerId -> Object -> Bool
isObjectEnemy Nothing _ = False
isObjectEnemy (Just p) Object{..} =
  case _objectPlayer of
    Nothing -> False
    Just op ->
      if isPlayerGaia op
        then False
        else op /= p

isObjectFriend :: Maybe PlayerId -> Object -> Bool
isObjectFriend Nothing _ = False
isObjectFriend (Just p) Object{..} =
  case _objectPlayer of
    Nothing -> False
    Just op ->
      if isPlayerGaia op
        then False
        else op == p


{-

just some helpful specicalisations
-}

isVillager :: HasObjectRestrict a => a -> Bool
isVillager a = getObjectType a == Just OT_Villager

isHerdable :: HasObjectRestrict a => a -> Bool
isHerdable a = getObjectType a == Just OT_Sheep

isBuilding :: HasObjectRestrict a => a -> Bool
isBuilding a = anyMeetsRestriction a OTRestrictionIsBuilding

isMonk :: HasObjectRestrict a => a -> Bool
isMonk a = anyMeetsRestriction a OTRestrictionIsMonk

isUnit :: HasObjectRestrict a => a -> Bool
isUnit a = anyMeetsRestriction a OTRestrictionIsUnit

isMilitaryUnit :: HasObjectRestrict a => a -> Bool
isMilitaryUnit a = anyMeetsRestriction a OTRestrictionIsMilitaryUnit

isNotDropoffBuilding :: HasObjectRestrict a => a -> Bool
isNotDropoffBuilding a = anyMeetsRestriction a OTRestrictionIsNotDropoffBuilding

isResource :: HasObjectRestrict a => a -> Bool
isResource a = anyMeetsRestriction a OTRestrictionIsResource

{-isObjectResource :: Object -> Bool
isObjectResource Object{..} =
  case objectInfo of
    ObjectInfoMapObject u -> isResource $ mapObjectType u
    ObjectInfoUnit u ->
      case toObjectType u of
        Nothing -> False
        Just n -> and $ map isResource $ NE.toList n
    _ -> False

isObjectPrimaryActableByPlayerMilitary :: Maybe PlayerId -> Object -> Bool
isObjectPrimaryActableByPlayerMilitary mp Object{..} =
  case objectInfo of
    ObjectInfoMapObject u -> canMilitaryPrimaryAct $ mapObjectType u
    ObjectInfoBuilding _ ->
      case mp of
        Nothing -> True
        Just p ->
          if (objectPlayer == Just p)
            then False
            else True
    _ -> True -- monks can target own units


restrictBuildingType :: BuildingType -> NonEmpty ObjectType -> BuildingType
restrictBuildingType BuildingTypeUnknown new = BuildingTypeOneOf $ new
restrictBuildingType (BuildingTypeOneOf old) new =
  case filter (\t -> t `elemNonEmpty` new) $ NE.toList old of
    [] -> error $ "Expected to find at least one matching building type in " ++ show old ++ " when restricting to " ++ show new
    [x] -> BuildingTypeKnown x
    xs -> BuildingTypeOneOf $ nonEmptyPartial xs
restrictBuildingType (BuildingTypeKnown t) new =
  case t `elemNonEmpty` new of
    True -> BuildingTypeKnown t
    False -> error $ "Expected known building to match " ++ show new ++ " but it was a " ++ show t


isVillagerType :: ObjectType -> Bool
isVillagerType OT_Villager = True
isVillagerType _ = False

isNotVillagerOrMilitary :: ObjectType -> Bool
isNotVillagerOrMilitary OT_Sheep = True
isNotVillagerOrMilitary _ = False



-}

