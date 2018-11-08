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
    (OT_LumberCamp, [Tech_DoubleBitAxe])
  , (OT_Barracks, [Tech_ManAtArms])
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
  , (OT_Barracks, [OT_ManAtArms])
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