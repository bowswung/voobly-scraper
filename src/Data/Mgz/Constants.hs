module Data.Mgz.Constants (
  module Data.Mgz.Constants.Techs,
  module Data.Mgz.Constants.Objects,
  buildingToTechMap,
  techToBuildingMap,
  buildingToTrainUnitMap,
  trainUnitToBuildingMap
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