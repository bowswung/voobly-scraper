module Data.Mgz.Simulate.Render where

import RIO

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import qualified RIO.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict
import Data.Proxy(Proxy(..))
import qualified Data.Text.Format as F
import qualified Data.Text.Buildable as F.Buildable
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified RIO.HashMap as HM



import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.State
import Data.Mgz.Simulate.Events



renderAllObjects :: Sim TL.Text
renderAllObjects = do
  ss <- get
  t <- (flip mapM) (IxSet.toAscList (Proxy :: Proxy ObjectId) $ objects (gameState ss)) $ \Object{..} -> do
    oRen <- renderObject $ objectId
    pure $ rPad 10 (objectIdToInt objectId) <> oRen
  pure $ TL.intercalate "\n" $ map TL.toLazyText t




renderEvents :: Sim TL.Text
renderEvents = do
  ss <- get
  t <- mapM renderEvent $ IxSet.toAscList (Proxy :: Proxy EventId) $ events (gameState ss)
  pure $ TL.intercalate "\n" $ map TL.toLazyText t

rPad ::  (F.Buildable.Buildable a) => Int -> a-> TL.Builder
rPad i a = F.right i ' ' a

renderEvent :: Event -> Sim  TL.Builder
renderEvent e@Event{..} = do
  d <- detail
  p <- renderPlayer eventPlayerResponsible
  let assignedIds = if (length $ eventObjectIdAssignmentIdx e) > 0 then " assignedIds" <> displayShowB (eventObjectIdAssignmentIdx e) else ""
  pure $ simOrReal <> " " <> rPad 9 (eventIdToInt eventId) <> rPad 12 p <> d <> assignedIds
  where

    detail :: Sim  TL.Builder
    detail =
      case eventType of
        (EventTypePrimary (EventPrimary{..})) -> do
          t <- renderObject eventPrimaryTarget
          u <- renderObjects eventPrimaryObjects
          pure $ "Primaried " <> t <> " with " <> u <> " at " <> renderPos eventPrimaryPos
        (EventTypeGather (EventGather{..})) -> do
          t <- renderObject eventGatherTargetId
          u <- renderObjects eventGatherGatherers
          pure $ "Gathered " <> t <> " with " <> u <> " at " <> renderPos eventGatherPos
        (EventTypeGatherRelic (EventGatherRelic{..})) -> do
          t <- renderObject eventGatherRelicTargetId
          u <- renderObjects eventGatherRelicGatherers
          pure $ "Gathered relic " <> t <> " with " <> u <> " at " <> renderPos eventGatherRelicPos

        (EventTypeAttack (EventAttack{..})) -> do
          t <- renderObject eventAttackTargetId
          u <- renderObjects eventAttackAttackers
          pure $ "Attacked " <> t <> " with " <> u <> " at " <> renderPos eventAttackPos
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
          pure $ "Placed " <> renderBuildingType eventBuildingType <> " at " <> renderPos eventBuildPos <> " with " <> u
        (EventTypeResearch (EventResearch{..})) -> do
          t <- renderObject eventResearchBuilding
          pure $ "Researched " <> displayShowB eventResearchTech <> " at " <> t
        (EventTypeTrain (EventTrain{..})) -> do
          t <- renderObject eventTrainBuilding
          pure $ "Trained " <> displayShowB eventTrainNumber <> " " <> renderUnitType eventTrainType  <> "s at " <> t
        (EventTypeStopGeneral (EventStopGeneral{..})) -> do
          u <- renderObjects eventStopSelectedIds
          pure $ "Stopped " <> u
        (EventTypeWaypoint (EventWaypoint{..})) -> do
          u <- renderObjects eventWaypointSelectedObjects
          pure $ "Waypointed " <> u <> " to " <> renderPosSimple eventWaypointPos
        (EventTypeRally (EventRally{..})) -> do
          b <- renderObjects eventRallyBuildings
          case eventRallyTargetObject of
            Nothing -> pure $ "Rallied " <> b <> " to map position " <> renderPos eventRallyPos
            Just t -> do
              tr <- renderObject t
              pure $ "Rallied " <> b <> " to " <> tr <> " at map position " <> renderPos eventRallyPos
        (EventTypeDelete (EventDelete{..})) -> do
          t <- renderObject eventDeleteObjectId
          pure $ "Deleted " <> t
        (EventTypeResign (EventResign{..})) -> pure $ "Resigned "
        (EventTypeAttackGround (EventAttackGround{..})) -> do
          u <- renderUnits eventAttackGroundUnitIds
          pure $ "Attacked the ground " <> " at " <> renderPos eventAttackGroundPos <> " with " <> u
        (EventTypeTribute (EventTribute{..})) -> do
          toPlayer <- renderPlayer $ Just eventTributeTo
          pure $ "Tributed " <> toPlayer <> " " <> (displayShowB eventTributeAmount) <> " " <> displayShowB eventTributeResourceKind <> " with a transaction fee of " <> displayShowB eventTributeTransationFee
        (EventTypeRepair (EventRepair{..})) -> do
          u <- renderUnits eventRepairRepairers
          o <- renderObject eventRepairRepaired
          pure $ "Repaired " <> o <> " with " <> u
        (EventTypeUngarrison (EventUngarrison{..})) -> do
          o <- renderObjects eventUngarrisonReleasedFrom
          pure $ "Ungarrisoned from " <> o
        (EventTypeToggleGate (EventToggleGate{..})) -> do
          o <- renderObject eventToggleGateGate
          pure $ "Toggled " <> o
        (EventTypeGarrison (EventGarrison{..})) -> do
          t <- renderObject eventGarrisonTargetId
          u <- renderUnits eventGarrisonGarrisonedUnits
          pure $ "Garrisoned " <> u <> " in " <> t
        (EventTypePackOrUnpack (EventPackOrUnpack{..})) -> do
          let pack = if eventPackOrUnpackPacked then "Packed" else "Unpacked"
          u <- renderUnits eventPackOrUnpackTrebuchets
          pure $ pack <> " " <> u
        (EventTypeUseMarket (EventUseMarket{..})) -> do

          let bors = if eventUseMarketBuyOrSell == Buy then "Bought" else "Sold"
          t <- renderObject eventUseMarketMarket
          pure $ bors <> " " <> displayShowB (eventUseMarketAmount * 100) <> " " <> displayShowB eventUseMarketKind <> " at " <> t
        (EventTypeDropRelic (EventDropRelic{..})) -> do
          o <- renderObject eventDropRelicMonkId
          pure $ "Dropped a relic with " <> o
        (EventTypeTownBell (EventTownBell{..})) -> do
          o <- renderObject eventTownBellTownCenter
          let r = if eventTownBellActive then "Rang" else "Unrang"
          pure $ r <> " the town bell at " <> o
        (EventTypeBackToWork (EventBackToWork{..})) -> do
          o <- renderObject eventBackToWorkBuildingId
          pure $ "Sent the units inside " <> o <> " back to work"
        (EventTypeWall (EventWall{..})) -> do
          o <- renderUnits eventWallBuilders
          pure $ "Walled with " <> renderRawObjectType eventWallBuildingType <> " from " <> renderPosSimple eventWallStartPos <> " to " <> renderPosSimple eventWallEndPos <> " with " <> o

        --_ -> pure $ displayShowB eventType
    simOrReal :: TL.Builder
    simOrReal =
      case eventKind of
        EventKindReal _ -> "R"
        EventKindSimulated -> "S"


renderPlayer :: Maybe PlayerId -> Sim TL.Builder
renderPlayer Nothing = pure "Unknown"
renderPlayer (Just pid) = do
  p <- fmap (HM.lookup pid . playerInfos . gameState) get
  pure $ maybe "NOTFOUND" (F.Buildable.build . playerInfoName) p

renderPos :: Pos -> TL.Builder
renderPos (Pos x y) = "(" <> displayShowB x <> ", " <> displayShowB y <> ")"

renderPosSimple :: PosSimple -> TL.Builder
renderPosSimple (PosSimple x y) = "(" <> displayShowB x <> ", " <> displayShowB y <> ")"

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
                ObjectInfoBuilding a -> do
                  b <- renderBuilding a
                  pure $ b <> " (" <> displayShowB (objectIdToInt objectId) <> ")"
                ObjectInfoMapObject a -> renderMapObject a
                ObjectInfoUnknown t ->  pure $ maybe "Unknown object" (displayShowB) t
      if fmap isGaia objectPlayer == Just True
        then pure $ "a " <> t
        else do
          belong <- renderPlayer objectPlayer
          pure $ "a " <> t <> " (" <> (displayShowB . objectIdToInt $ objectId) <> ") " <> " belonging to " <> belong

renderObjectType :: (ToObjectId a) => a -> Sim TL.Builder
renderObjectType oid = do
  m <- lookupObject oid
  case m of
    Nothing -> pure $ "NOT FOUND"
    Just Object{..} -> do
      t <- case objectInfo of
            ObjectInfoUnit a -> renderUnit a
            ObjectInfoBuilding a -> renderBuilding a
            ObjectInfoMapObject a -> renderMapObject a
            ObjectInfoUnknown t -> pure $ maybe "Unknown" (displayShowB) t
      p <- renderPlayer objectPlayer
      pure $ p <> " " <> t <> "(" <> (displayShowB . objectIdToInt . toObjectId $ oid) <> ")"

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
renderUnit (Unit{..}) = pure $ renderUnitType unitType


renderBuilding :: Building -> Sim TL.Builder
renderBuilding (Building{..}) = pure $ renderBuildingType buildingType


renderBuildingType :: BuildingType -> TL.Builder
renderBuildingType (BuildingTypeKnown ot) = renderRawObjectType ot
renderBuildingType (BuildingTypeOneOf a) = renderObjectTypes $ NE.toList a
renderBuildingType BuildingTypeUnknown = "UnknownBuilding"

renderUnitType :: UnitType -> TL.Builder
renderUnitType UnitTypeUnknown = "UnknownUnit"
renderUnitType UnitTypeVillager = "Villager"
renderUnitType (UnitTypeOther a) = renderRawObjectType a
renderUnitType (UnitTypeMilitary m) = renderMilitaryType m

renderMilitaryType :: MilitaryType -> TL.Builder
renderMilitaryType MilitaryTypeUnknown = "UnknownMilitary"
renderMilitaryType (MilitaryTypeKnown a) = renderRawObjectType a
renderMilitaryType (MilitaryTypeOneOf a) = renderObjectTypes $ NE.toList a

renderRawObjectType :: ObjectType -> TL.Builder
renderRawObjectType = F.Buildable.build . objectTypeToText

objectTypeToText :: ObjectType -> Text
objectTypeToText ot =  T.drop 3 $ displayShowT ot

renderObjectTypes :: [ObjectType] -> TL.Builder
renderObjectTypes ots = F.Buildable.build $ T.intercalate "|" $ map objectTypeToText ots

renderMapObject :: MapObject -> Sim TL.Builder
renderMapObject (MapObject{..}) = pure $ renderRawObjectType mapObjectType

displayShowB :: (Show a) => a -> TL.Builder
displayShowB = F.Buildable.build . displayShowT

displayShowTL ::  Show a => a -> TL.Text
displayShowTL = utf8BuilderToLazyText . displayShow