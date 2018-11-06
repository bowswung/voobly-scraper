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

newtype UnitId = UnitId Int deriving (Show, Eq, Ord)
newtype BuildingId = BuildingId Int deriving (Show, Eq, Ord)
newtype PlayerId = PlayerId Int deriving (Show, Eq, Ord)

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

data Action = Action {
  actionTime :: Int
} deriving (Show, Eq, Ord)

data Unit = Unit {
  unitId :: UnitId,
  unitType :: UnitType,
  unitOwner :: Maybe PlayerId,
  unitHistory :: [Action]
} deriving (Show, Eq, Ord)

data Building = Building {
  buildingId :: BuildingId,
  buildingType :: BuildingType,
  buildingOwner :: Maybe PlayerId,
  buildingHistory :: [Action]
} deriving (Show, Eq, Ord)

makeSimpleIxSet "UnitSet" ''Unit ['unitId, 'unitType]
makeSimpleIxSet "BuildingSet" ''Building ['buildingId, 'buildingType]


data GameState = GameState {
  units :: UnitSet,
  buildings :: BuildingSet
} deriving Show

simulate :: HasLogFunc env => RecInfo -> RIO env GameState
simulate RecInfo{..} = do
  logInfo "Start simulating"
  logInfo "Building histories"
  let initialGS = GameState IxSet.empty IxSet.empty

  let sWithHistories = snd $ execState (mapM addToHistory recInfoOps) (0, initialGS)
  logInfo $ "Found units: " <> displayShow (IxSet.size . units $ sWithHistories )
  logInfo $ "Found buildings: " <> displayShow (IxSet.size . buildings $ sWithHistories )



  pure $ error "Nothing"

type Ticks = Int

addToHistory :: Op -> State (Ticks, GameState) ()
addToHistory (OpTypeSync OpSync{..}) = modify' (\(t,s)-> (t + opSyncTime, s))
addToHistory (OpTypeCommand cmd) = addCommandToHistory cmd
addToHistory _ = pure ()


addCommandToHistory :: Command -> State (Ticks, GameState) ()
addCommandToHistory (CommandTypePrimary CommandPrimary{..}) = do



addCommandToHistory _ = pure ()

replay :: HasLogFunc env => GameState -> RIO env ()
replay _ =
  logInfo "Game start"
