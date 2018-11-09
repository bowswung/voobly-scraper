module Data.Mgz.Constants (
  module Data.Mgz.Constants,
  module Data.Mgz.Constants.Techs,
  module Data.Mgz.Constants.Objects,
)
  where
import Data.Mgz.Constants.Techs
import Data.Mgz.Constants.Objects
import qualified RIO.HashMap as HM
import RIO
import qualified RIO.List as L

buildingToTechMap :: HM.HashMap ObjectType [Tech]
buildingToTechMap = HM.fromList [
    (OT_LumberCamp, [Tech_DoubleBitAxe, Tech_TwoManSaw])
  , (OT_Barracks, [Tech_ManAtArms])
  , (OT_Castle, [Tech_Conscription, Tech_EliteWarWagon])
  , (OT_Mill, [Tech_HorseCollar,Tech_HeavyPlow])
  , (OT_Barracks, [Tech_Pikeman,Tech_Squires])
  , (OT_MiningCamp, [Tech_StoneMining,Tech_GoldShaftMining, Tech_GoldMining,Tech_StoneShaftMining ])
  , (OT_Blacksmith, [Tech_Fletching,Tech_BodkinArrow,Tech_PaddedArcherArmor,Tech_Bracer,Tech_LeatherArcherArmor,Tech_ScaleBardingArmor,Tech_Forging,Tech_ChainBardingArmor ])
  , (OT_TownCenter, [Tech_TownWatch, Tech_HandCart])
  , (OT_SiegeWorkshop, [Tech_CappedRam])
  , (OT_University, [Tech_Ballistics, Tech_Masonry])
  , (OT_Stable, [Tech_LightCavalry,Tech_Husbandry, Tech_Hussar])
  ]

techToBuildingMap :: HM.HashMap Tech [ObjectType]
techToBuildingMap = flipListHM buildingToTechMap

flipListHM :: (Hashable b, Eq b) => HM.HashMap a [b] -> HM.HashMap b [a]
flipListHM m =
  let ls = HM.toList m
      allPairs = concat $ map (\(ut, ts) -> map (\t -> (t, ut)) ts) ls
      grouped = map (\t -> (t, map snd (filter ((==) t . fst) allPairs ))) (L.nub $ map fst allPairs)
  in HM.fromList grouped


buildingToTrainUnitMap :: HM.HashMap ObjectType [ObjectType]
buildingToTrainUnitMap = HM.fromList [
    (OT_TownCenter, [OT_Villager])
  , (OT_Barracks, [OT_Militia, OT_ManAtArms, OT_Spearman])
  , (OT_ArcheryRange, [OT_Archer, OT_Skirmisher])
  , (OT_Castle, [OT_WarWagon, OT_TrebuchetPacked])
  , (OT_SiegeWorkshop, [OT_Mangonel,OT_BatteringRam])
  , (OT_Monastery, [OT_Monk])
  ]

trainUnitToBuildingMap ::  HM.HashMap ObjectType [ObjectType]
trainUnitToBuildingMap = flipListHM buildingToTrainUnitMap

attackingBuildingTypes :: [ObjectType]
attackingBuildingTypes = [
    OT_TownCenter
  , OT_Castle
  , OT_WatchTower
  ]
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

isResourceOrRelic :: ObjectType -> Bool
isResourceOrRelic ot = isRelic ot || isResource ot

isResource :: ObjectType -> Bool
isResource ot = HM.member ot objectTypeToResourceKindMap

isRelic :: ObjectType -> Bool
isRelic OT_Relic = True
isRelic _ = False

canMilitaryPrimaryAct :: ObjectType -> Bool
canMilitaryPrimaryAct OT_ForestTree = False
canMilitaryPrimaryAct OT_OakForestTree = False
canMilitaryPrimaryAct OT_ForageBush = False
canMilitaryPrimaryAct OT_GoldMine = False
canMilitaryPrimaryAct OT_StoneMine = False
canMilitaryPrimaryAct _ = True


-- witness the type of an object
data ObjectTypeW =
    ObjectTypeWUnit
  | ObjectTypeWBuilding
  | ObjectTypeWMapObject
  | ObjectTypeWUnknown
  deriving (Show, Eq, Ord)


objectTypeToObjectTypeW :: ObjectType -> ObjectTypeW
objectTypeToObjectTypeW OT_Castle = ObjectTypeWBuilding
objectTypeToObjectTypeW ot = error $ "objectTypeToObjectTypeW : Nothing defined for ot " ++ show ot