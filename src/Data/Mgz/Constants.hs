module Data.Mgz.Constants (
  module Data.Mgz.Constants,
  module Data.Mgz.Constants.Techs,
  module Data.Mgz.Constants.Objects,
)
  where
import qualified RIO.HashMap as HM
import RIO
import qualified RIO.List as L
import Prelude(enumFrom)

import Data.List.NonEmpty(NonEmpty(..))
import Data.Mgz.Constants.Techs
import Data.Mgz.Constants.Objects

newtype PlayerId = PlayerId Int deriving (Show, Eq, Ord, Generic) -- 1,2 etc - same as the rec file
instance Hashable PlayerId

flipListHM :: (Hashable b, Eq b) => HM.HashMap a [b] -> HM.HashMap b [a]
flipListHM m =
  let ls = HM.toList m
      allPairs = concat $ map (\(ut, ts) -> map (\t -> (t, ut)) ts) ls
      grouped = map (\t -> (t, map snd (filter ((==) t . fst) allPairs ))) (L.nub $ map fst allPairs)
  in HM.fromList grouped

hmMatches :: (Eq a, Hashable a, Eq b) => a -> b -> HM.HashMap a [b] -> Bool
hmMatches k v hm =
  case HM.lookup k hm of
    Nothing -> False
    Just vs -> v `elem` vs


buildingToTechMap :: HM.HashMap ObjectType [Tech]
buildingToTechMap = HM.fromList [
    (OT_LumberCamp, [Tech_DoubleBitAxe, Tech_TwoManSaw])
  , (OT_Barracks, [Tech_ManAtArms])
  , (OT_Castle, [Tech_Conscription, Tech_EliteWarWagon])
  , (OT_Mill, [Tech_HorseCollar,Tech_HeavyPlow])
  , (OT_Barracks, [Tech_Pikeman,Tech_Squires])
  , (OT_MiningCamp, [Tech_StoneMining,Tech_GoldShaftMining, Tech_GoldMining,Tech_StoneShaftMining ])
  , (OT_Blacksmith, [Tech_Fletching,Tech_BodkinArrow,Tech_PaddedArcherArmor,Tech_Bracer,Tech_LeatherArcherArmor,Tech_ScaleBardingArmor,Tech_Forging,Tech_ChainBardingArmor ])
  , (OT_TownCenter, [Tech_TownWatch, Tech_HandCart, Tech_Loom,Tech_FeudalAge,Tech_Wheelbarrow,Tech_CastleAge,Tech_ImperialAge])
  , (OT_SiegeWorkshop, [Tech_CappedRam])
  , (OT_University, [Tech_Ballistics, Tech_Masonry])
  , (OT_Stable, [Tech_LightCavalry,Tech_Husbandry, Tech_Hussar])
  , (OT_ArcheryRange, [Tech_Crossbowman])
  , (OT_Market, [Tech_Cartography])
  ]

techToBuildingMap :: HM.HashMap Tech [ObjectType]
techToBuildingMap = flipListHM buildingToTechMap
buildingToTrainUnitMap :: HM.HashMap ObjectType [ObjectType]
buildingToTrainUnitMap = HM.fromList [
    (OT_TownCenter, [OT_Villager])
  , (OT_Barracks, [OT_Militia, OT_ManAtArms, OT_Spearman])
  , (OT_Stable, [OT_ScoutCavalry])
  , (OT_ArcheryRange, [OT_Archer, OT_Skirmisher])
  , (OT_Castle, [OT_WarWagon, OT_TrebuchetPacked, OT_Trebuchet])
  , (OT_SiegeWorkshop, [OT_Mangonel,OT_BatteringRam])
  , (OT_Dock, [OT_FishingShip])
  , (OT_Monastery, [OT_Monk])
  , (OT_Market, [OT_TradeCart])
  ]

trainUnitToBuildingMap ::  HM.HashMap ObjectType [ObjectType]
trainUnitToBuildingMap = flipListHM buildingToTrainUnitMap


data ResourceKind =
    ResourceKindFood
  | ResourceKindWood
  | ResourceKindGold
  | ResourceKindStone
  deriving (Show, Eq, Ord, Generic)
instance Hashable ResourceKind

resourceKindToObjectTypeMap :: HM.HashMap ResourceKind [ObjectType]
resourceKindToObjectTypeMap = HM.fromList [
    (ResourceKindWood, [OT_ForestTree, OT_OakForestTree])
  , (ResourceKindFood, [OT_Sheep, OT_ForageBush, OT_Deer, OT_WildBoar])
  , (ResourceKindGold, [OT_GoldMine])
  , (ResourceKindStone, [OT_StoneMine])
  ]

objectTypeToResourceKindMap :: HM.HashMap ObjectType ResourceKind
objectTypeToResourceKindMap =
    let ls = HM.toList resourceKindToObjectTypeMap
        allPairs = concat $ map (\(ut, ts) -> map (\t -> (t, ut)) ts) ls
  in HM.fromList allPairs


isPlayerGaia :: PlayerId -> Bool
isPlayerGaia (PlayerId 0) = True
isPlayerGaia _ = False


objectPartsNumber :: ObjectType -> Int
objectPartsNumber OT_TownCenter = 4
objectPartsNumber _ = 1


-- witness the type of an object
data ObjectTypeW =
    ObjectTypeWUnit
  | ObjectTypeWBuilding
  | ObjectTypeWMapObject
  | ObjectTypeWUnknown
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Hashable ObjectTypeW


-- this might be better as a more flexible dsl! that way it can also cover specific object types etc and we can have one unified interface
data OTRestriction =
    OTRestrictionCanAttack
  | OTRestrictionIsRepairable
  | OTRestrictionIsUngarrisonable
  | OTRestrictionIsGarrisonable
  | OTRestrictionIsUnit
  | OTRestrictionCanGarrisonCastle
  | OTRestrictionCanGarrisonTCEtc
  | OTRestrictionIsMilitaryUnit
  | OTRestrictionCanAttackGround
  | OTRestrictionIsGatherer
  | OTRestrictionIsLandResourceGatherer
  | OTRestrictionIsBuilder
  | OTRestrictionIsMonk
  | OTRestrictionIsBuilding
  | OTRestrictionIsNotDropoffBuilding
  | OTRestrictionIsMapObject
  | OTRestrictionIsNotActableByMilitary
  | OTRestrictionIsResource
  | OTRestrictionIsLandResource
  | OTRestrictionIsRelic
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Hashable OTRestriction

restrictionToKnownObjectTypeW :: OTRestriction -> Maybe ObjectTypeW
restrictionToKnownObjectTypeW OTRestrictionIsGatherer = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsLandResourceGatherer = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsBuilder = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsMonk = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionCanAttack = Nothing
restrictionToKnownObjectTypeW OTRestrictionIsBuilding = pure ObjectTypeWBuilding
restrictionToKnownObjectTypeW OTRestrictionIsUnit = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsMilitaryUnit = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsResource = pure ObjectTypeWMapObject
restrictionToKnownObjectTypeW OTRestrictionIsLandResource = pure ObjectTypeWMapObject
restrictionToKnownObjectTypeW OTRestrictionIsRelic = pure ObjectTypeWMapObject
restrictionToKnownObjectTypeW OTRestrictionIsMapObject = pure ObjectTypeWMapObject
restrictionToKnownObjectTypeW OTRestrictionCanAttackGround = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionCanGarrisonTCEtc = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionCanGarrisonCastle = pure ObjectTypeWUnit
restrictionToKnownObjectTypeW OTRestrictionIsRepairable = Nothing
restrictionToKnownObjectTypeW OTRestrictionIsGarrisonable = Nothing
restrictionToKnownObjectTypeW OTRestrictionIsUngarrisonable = Nothing
restrictionToKnownObjectTypeW OTRestrictionIsNotActableByMilitary = Nothing
restrictionToKnownObjectTypeW OTRestrictionIsNotDropoffBuilding = pure ObjectTypeWBuilding





restrictionToObjectTypes :: OTRestriction -> [ObjectType]
restrictionToObjectTypes OTRestrictionIsGatherer = [OT_Villager, OT_FishingShip]
restrictionToObjectTypes OTRestrictionIsLandResourceGatherer = [OT_Villager]
restrictionToObjectTypes OTRestrictionIsBuilder = [OT_Villager]
restrictionToObjectTypes OTRestrictionIsMonk = [OT_Monk]
restrictionToObjectTypes OTRestrictionIsRepairable = allBuildings ++ allSiege ++ allBoats ++ (concat $ catMaybes [HM.lookup OT_Market buildingToTrainUnitMap])
restrictionToObjectTypes OTRestrictionIsUngarrisonable = L.nub $ restrictionToObjectTypes OTRestrictionIsGarrisonable ++ HM.keys buildingToTrainUnitMap
restrictionToObjectTypes OTRestrictionIsGarrisonable = L.nub $ garrisonableBuildings ++ garrisonableSiege ++ garrisonableBoats
restrictionToObjectTypes OTRestrictionIsBuilding = allBuildings
restrictionToObjectTypes OTRestrictionCanAttack = L.nub $ attackingBuildings ++ allMilitaryUnits ++ [OT_Villager]
restrictionToObjectTypes OTRestrictionIsUnit = allUnits
restrictionToObjectTypes OTRestrictionIsMilitaryUnit = allMilitaryUnits
restrictionToObjectTypes OTRestrictionIsResource = allResourceTypes
restrictionToObjectTypes OTRestrictionIsLandResource = landResourceTypes
restrictionToObjectTypes OTRestrictionIsRelic = [OT_Relic]
restrictionToObjectTypes OTRestrictionIsNotActableByMilitary = notActableByMilitaryTypes
restrictionToObjectTypes OTRestrictionIsMapObject = allMapObjects
restrictionToObjectTypes OTRestrictionCanAttackGround = attackGroundUnits
restrictionToObjectTypes OTRestrictionIsNotDropoffBuilding = filter (\o -> not $ o `elem` dropoffBuildings) $ allBuildings
restrictionToObjectTypes OTRestrictionCanGarrisonTCEtc = footUnits ++ [OT_Villager]
restrictionToObjectTypes OTRestrictionCanGarrisonCastle = (restrictionToObjectTypes OTRestrictionCanGarrisonTCEtc ) ++ horseUnits



repairableUnits :: [ObjectType]
repairableUnits = L.nub $ L.intersect (restrictionToObjectTypes OTRestrictionIsRepairable) allUnits


restrictionToObjectTypeMap :: HM.HashMap OTRestriction [ObjectType]
restrictionToObjectTypeMap = HM.fromList $ map (\r -> (r, restrictionToObjectTypes r)) (enumFrom minBound)

objectTypesForRestrictions :: NonEmpty OTRestriction -> [ObjectType]
objectTypesForRestrictions (firstR:|rs) = foldl' (\ots r -> L.nub $ L.intersect ots (restrictionToObjectTypes r)) (L.nub $ restrictionToObjectTypes firstR) rs

objectTypeToRestrictionMap :: HM.HashMap ObjectType [OTRestriction]
objectTypeToRestrictionMap = flipListHM restrictionToObjectTypeMap


objectTypeWToObjectTypeMap :: HM.HashMap ObjectTypeW [ObjectType]
objectTypeWToObjectTypeMap = HM.fromList $ [
    (ObjectTypeWUnit, restrictionToObjectTypes OTRestrictionIsUnit)
  , (ObjectTypeWBuilding, restrictionToObjectTypes OTRestrictionIsBuilding)
  , (ObjectTypeWMapObject, restrictionToObjectTypes OTRestrictionIsMapObject)
  ]

objectTypeToObjectTypeWMap :: HM.HashMap ObjectType ObjectTypeW
objectTypeToObjectTypeWMap =
    let ls = HM.toList objectTypeWToObjectTypeMap
        allPairs = concat $ map (\(ut, ts) -> map (\t -> (t, ut)) ts) ls
  in HM.fromList allPairs


lookupObjectTypeRestrictions :: ObjectType -> [OTRestriction]
lookupObjectTypeRestrictions ot =
  case HM.lookup ot objectTypeToRestrictionMap of
    Just w -> w
    Nothing -> error $ "Could not find an OTRestriction entry for object type " ++ show ot


lookupObjectTypeW :: ObjectType -> ObjectTypeW
lookupObjectTypeW ot =
  case HM.lookup ot objectTypeToObjectTypeWMap of
    Just w -> w
    Nothing -> error $ "Could not find an ObjectTypeW for object type " ++ show ot


allUnits :: [ObjectType]
allUnits = [OT_Sheep] ++ (concat $ HM.elems buildingToTrainUnitMap)

nonMilitaryUnits :: [ObjectType]
nonMilitaryUnits = [OT_Villager, OT_FishingShip, OT_Sheep, OT_TradeCart, OT_TradeCog]

allMilitaryUnits :: [ObjectType]
allMilitaryUnits = filter (\t -> not $ t `elem` nonMilitaryUnits) allUnits

footUnits :: [ObjectType]
footUnits = (concat $ catMaybes [HM.lookup OT_Barracks buildingToTrainUnitMap]) ++ [OT_Villager, OT_Archer, OT_Skirmisher]

horseUnits :: [ObjectType]
horseUnits = (concat $ catMaybes [HM.lookup OT_Stable buildingToTrainUnitMap]) ++ [OT_CavalryArcher, OT_WarWagon]

attackGroundUnits :: [ObjectType]
attackGroundUnits = [OT_Trebuchet, OT_TrebuchetPacked, OT_Mangonel]
allBuildings :: [ObjectType]
allBuildings = L.nub $ HM.keys buildingToTechMap ++ HM.keys buildingToTrainUnitMap ++ nonActingBuildings ++ attackingBuildings

nonActingBuildings :: [ObjectType]
nonActingBuildings = [OT_House, OT_Farm, OT_PalisadeWall, OT_StoneWall, OT_PalisadeGate, OT_Gate]

allSiege :: [ObjectType]
allSiege = [OT_Trebuchet] ++ (concat $ catMaybes [HM.lookup OT_SiegeWorkshop buildingToTrainUnitMap])

allBoats :: [ObjectType]
allBoats = concat $ catMaybes [HM.lookup OT_Dock buildingToTrainUnitMap]

garrisonableBuildings :: [ObjectType]
garrisonableBuildings = [OT_TownCenter, OT_Castle, OT_WatchTower]

garrisonableSiege :: [ObjectType]
garrisonableSiege = [OT_BatteringRam, OT_SiegeTower]

garrisonableBoats :: [ObjectType]
garrisonableBoats = [OT_TransportShip]

attackingBuildings :: [ObjectType]
attackingBuildings = [OT_TownCenter, OT_Castle, OT_WatchTower]

dropoffBuildings :: [ObjectType]
dropoffBuildings = [OT_TownCenter, OT_LumberCamp, OT_Mill, OT_MiningCamp, OT_Dock]

allResourceTypes :: [ObjectType]
allResourceTypes = HM.keys objectTypeToResourceKindMap

allMapObjects :: [ObjectType]
allMapObjects = allResourceTypes ++ [OT_Cactus, OT_Wolf, OT_Hawk, OT_Harbor, OT_ExtraneousPart OT_TownCenter, OT_Flare]

waterResourceTypes :: [ObjectType]
waterResourceTypes = [OT_ShoreFish, OT_GreatFishMarlin]

landResourceTypes :: [ObjectType]
landResourceTypes = filter (\t -> not $ t `elem` waterResourceTypes) allResourceTypes

notActableByMilitaryTypes :: [ObjectType]
notActableByMilitaryTypes = [
  OT_ForestTree,
  OT_OakForestTree,
  OT_ForageBush,
  OT_GoldMine,
  OT_StoneMine
  ]


canObjectTypeOverlap :: ObjectType ->Bool
canObjectTypeOverlap OT_Flare = True
canObjectTypeOverlap _ = False