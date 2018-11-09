module Data.Mgz.Simulate.Objects where

import RIO


import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE


import Data.Mgz.Deserialise
import Data.Mgz.Constants
import Data.Mgz.Utils

newtype EventId = EventId {eventIdToInt :: Int} deriving (Show, Eq, Ord) -- sequence

newtype UnitId = UnitId ObjectId deriving (Show, Eq, Ord)

unitId :: ObjectUnit -> UnitId
unitId (ObjectUnit o) = UnitId . objectId $ o

newtype BuildingId = BuildingId ObjectId deriving (Show, Eq, Ord)

buildingId :: ObjectBuilding -> BuildingId
buildingId (ObjectBuilding o) = BuildingId . objectId $ o




data Object = Object {
  objectId :: ObjectId,
  objectPlayer :: Maybe PlayerId,
  objectInfo :: ObjectInfo,
  objectPlacedByGame :: Bool
} deriving (Show, Eq, Ord)

newtype ObjectUnit = ObjectUnit Object -- just for the types
newtype ObjectBuilding = ObjectBuilding Object -- just for the types

-- witness the type of an object
data ObjectTypeW =
    ObjectTypeWUnit
  | ObjectTypeWBuilding
  | ObjectTypeWMapObject
  | ObjectTypeWUnknown
  deriving (Show, Eq, Ord)

objectTypeW :: Object -> ObjectTypeW
objectTypeW o =
  case objectInfo o of
    ObjectInfoUnit _ -> ObjectTypeWUnit
    ObjectInfoBuilding _ -> ObjectTypeWBuilding
    ObjectInfoMapObject _ -> ObjectTypeWMapObject
    ObjectInfoUnknown _ -> ObjectTypeWUnknown


objectTypeToObjectTypeW :: ObjectType -> ObjectTypeW
objectTypeToObjectTypeW OT_Castle = ObjectTypeWBuilding
objectTypeToObjectTypeW ot = error $ "objectTypeToObjectTypeW : Nothing defined for ot " ++ show ot

data ObjectInfo =
    ObjectInfoUnit Unit
  | ObjectInfoBuilding Building
  | ObjectInfoMapObject MapObject

  | ObjectInfoUnknown (Maybe ObjectType)
  deriving (Show, Eq, Ord)

data Unit = Unit {
  unitType :: UnitType
} deriving (Show, Eq, Ord)

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


data Building = Building {
  buildingType :: BuildingType,
  buildingPos :: Maybe Pos,
  buildingPlaceEvent :: Maybe EventId
} deriving (Show, Eq, Ord)

data BuildingType =
    BuildingTypeUnknown
  | BuildingTypeKnown ObjectType
  | BuildingTypeOneOf (NonEmpty ObjectType)
  deriving (Show, Eq, Ord)



data MapObject = MapObject {
  mapObjectType :: ObjectType,
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

instance ToObjectId UnitId where
  toObjectId (UnitId i) = i

instance ToObjectId BuildingId where
  toObjectId (BuildingId i) = i


class HasObjectType a where
  toObjectType :: a -> Maybe (NonEmpty ObjectType)
  setObjectType :: a -> ObjectType -> a

instance HasObjectType Object where
  toObjectType o =
    case objectInfo o of
      ObjectInfoUnit u -> toObjectType u
      ObjectInfoBuilding u -> toObjectType u
      ObjectInfoMapObject u -> toObjectType u
      ObjectInfoUnknown u -> mNonEmpty u
  setObjectType o t =
    let ni =
          case objectInfo o of
            ObjectInfoUnit u -> ObjectInfoUnit $ setObjectType u t
            ObjectInfoBuilding u -> ObjectInfoBuilding $ setObjectType u t
            ObjectInfoMapObject u -> ObjectInfoMapObject $ setObjectType u t
            ObjectInfoUnknown _ -> ObjectInfoUnknown $ Just t
    in o{objectInfo = ni}

instance HasObjectType Unit where
  toObjectType u = objectTypeFromUnitType (unitType u)
  setObjectType u t = u{unitType = objectTypeToUnitType t}

instance HasObjectType Building where
  toObjectType u = objectTypeFromBuildingType (buildingType u)
  setObjectType u t = u{buildingType = BuildingTypeKnown t}

instance HasObjectType MapObject where
  toObjectType u = NE.nonEmpty [mapObjectType u]
  setObjectType _ _ = error "All map objects should exist from the beginning"



objectTypeFromUnitType :: UnitType -> Maybe (NonEmpty ObjectType)
objectTypeFromUnitType (UnitTypeMilitary m) = objectTypeFromMilitaryType m
objectTypeFromUnitType (UnitTypeOther o) = NE.nonEmpty [o]
objectTypeFromUnitType UnitTypeVillager = NE.nonEmpty [OT_Villager]
objectTypeFromUnitType UnitTypeUnknown = Nothing

objectTypeFromMilitaryType :: MilitaryType -> Maybe (NonEmpty ObjectType)
objectTypeFromMilitaryType MilitaryTypeUnknown = Nothing
objectTypeFromMilitaryType (MilitaryTypeKnown o) = NE.nonEmpty [o]
objectTypeFromMilitaryType (MilitaryTypeOneOf a) = pure $ a

objectTypeFromBuildingType :: BuildingType -> Maybe (NonEmpty ObjectType)
objectTypeFromBuildingType BuildingTypeUnknown = Nothing
objectTypeFromBuildingType (BuildingTypeKnown m) = NE.nonEmpty [m]
objectTypeFromBuildingType (BuildingTypeOneOf o) = pure o




objectTypeToUnitType :: ObjectType -> UnitType
objectTypeToUnitType t =
  if isVillagerType t
    then UnitTypeVillager
    else if isNotVillagerOrMilitary t
     then UnitTypeOther t
     else UnitTypeMilitary . MilitaryTypeKnown $ t


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




extractBuilding :: Object -> Building
extractBuilding Object{..} =
  case objectInfo of
    ObjectInfoBuilding b -> b
    _ -> error "Could not extract building"


buildingObjectPos :: Object -> Maybe Pos
buildingObjectPos Object{..} =
  case objectInfo of
    ObjectInfoBuilding b -> buildingPos b
    _ -> Nothing







{-
extracting specific info

-}

{-
logic surrounding objects units and players
-}


doesObjectMatch :: Object -> (ObjectType -> Bool)-> Bool
doesObjectMatch o f =
  case fmap NE.toList $ toObjectType o of
    Just (x:xs) -> and $ map f (x:xs)
    _ -> False


isVillagerType :: ObjectType -> Bool
isVillagerType OT_Villager = True
isVillagerType _ = False

isNotVillagerOrMilitary :: ObjectType -> Bool
isNotVillagerOrMilitary OT_Sheep = True
isNotVillagerOrMilitary _ = False

isObjectVillager :: Object -> Bool
isObjectVillager Object{..} =
  case objectInfo of
    ObjectInfoUnit u -> unitType u == UnitTypeVillager
    _ -> False

-- @teams
isObjectEnemy :: Maybe PlayerId -> Object -> Bool
isObjectEnemy Nothing _ = False
isObjectEnemy (Just p) Object{..} =
  case objectPlayer of
    Nothing -> False
    Just op ->
      if isGaia op
        then False
        else op /= p

isObjectResource :: Object -> Bool
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




isGaia :: PlayerId -> Bool
isGaia (PlayerId 0) = True
isGaia _ = False