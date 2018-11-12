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
import qualified Data.Duration as D

import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.State
import Data.Mgz.Simulate.Events



renderAllObjects :: Sim TL.Text
renderAllObjects = do
  ss <- get
  t <- (flip mapM) (IxSet.toAscList (Proxy :: Proxy ObjectId) $ objects (gameState ss)) $ \o -> do
    oRen <- renderObject $ objectId o
    let poss = "    " <> renderMany (map (TL.toStrict .  TL.toLazyText . renderPos) (objectPosHistory o))
        del = case objectDeletedBy o of Nothing -> ""; Just e -> " Deleted by " <> displayShowB (eventIdToInt e)
    pure $ rPad 6 (objectIdToInt $ objectId o) <> rPad 10 (renderObjectTypeW $ objectTypeW o) <> oRen <> del <> poss
  pure $ TL.intercalate "\n" $ map TL.toLazyText t



renderTick :: Int -> TL.Builder
renderTick ticks  =
  let n = (fromIntegral ticks) * D.ms
      h = F.left 2 '0' $ displayShowB $ D.getHours n
      m = F.left 2 '0' $ displayShowB $ D.getMinutes ((n - D.hour * (fromIntegral $ D.getHours n)))
      s = F.left 2 '0' $ displayShowB $ D.getSeconds ((n - D.minute * (fromIntegral $ D.getMinutes n)))
      ms = F.left 3 '0' $ displayShowB $ D.getMs ((n - D.oneSecond * (fromIntegral $ D.getSeconds n)))
  in  h <> ":" <> m <> ":" <> s <> "." <> ms


renderEvents :: Sim TL.Text
renderEvents = do
  ss <- get
  t <- mapM renderEvent $ IxSet.toAscList (Proxy :: Proxy EventTick) $ events (gameState ss)
  pure $ TL.intercalate "\n" $ map TL.toLazyText t

rPad ::  (F.Buildable.Buildable a) => Int -> a-> TL.Builder
rPad i a = F.right i ' ' a

renderEvent :: Event -> Sim  TL.Builder
renderEvent e@Event{..} = do
  d <- detail
  p <- renderPlayer eventPlayerResponsible
  let assignedIds = if (length $ eventObjectIdAssignmentIdx e) > 0
                      then " [AssignedIds: " <> ( renderMany $ map (displayShowT . objectIdToInt .  objectIdFromEventObjectIdAssignmentIdx) (eventObjectIdAssignmentIdx e)) <> "]"
                      else ""
      t = renderTick eventTick

  pure $ simOrReal <> " " <> rPad 6 (eventIdToInt eventId) <> rPad 14 t <>  rPad 12 p <> d <> assignedIds
  where

    detail :: Sim  TL.Builder
    detail =
      case eventType of
        (EventTypePrimary (EventPrimary{..})) -> do
          t <- renderObject eventPrimaryTarget
          u <- renderObjects eventPrimaryObjects
          pure $ "Primaried a " <> t <> " with " <> u <> " at " <> renderPos eventPrimaryPos
        (EventTypeGather (EventGather{..})) -> do
          t <- renderObject eventGatherTargetId
          u <- renderObjects eventGatherGatherers
          pure $ "Gathered a " <> t <> " with " <> u <> " at " <> renderPos eventGatherPos
        (EventTypeVillOnRepairable (EventVillOnRepairable{..})) -> do
          t <- renderObject eventVillOnRepairableObject
          u <- renderObjects eventVillOnRepairableVills
          let ats = renderMany $ map eventTypeWToText (NE.toList eventVillOnRepairableType)
          pure $ ats <> " " <> t <> " with " <> u <> " at " <> renderPos eventVillOnRepairablePos
        (EventTypeGatherRelic (EventGatherRelic{..})) -> do
          t <- renderObject eventGatherRelicTargetId
          u <- renderObjects eventGatherRelicGatherers
          pure $ "Gathered relic " <> t <> " with " <> u <> " at " <> renderPos eventGatherRelicPos
        (EventTypeDropoffRelic (EventDropoffRelic{..})) -> do
          t <- renderObject eventDropoffRelicTargetId
          u <- renderObjects eventDropoffRelicGatherers
          pure $ "Dropped off relic at " <> t <> " with " <> u <> " at " <> renderPos eventDropoffRelicPos


        (EventTypeAttack (EventAttack{..})) -> do
          t <- renderObject eventAttackTargetId
          u <- renderObjects eventAttackAttackers
          pure $ "Attacked a " <> t <> " with " <> u <> " at " <> renderPos eventAttackPos
        (EventTypeMove (EventMove{..})) -> do
          u <- renderUnits eventMoveUnits
          pure $ "Moved " <> u <> " to " <> renderPos eventMovePos
        (EventTypeMilitaryDisposition (EventMilitaryDisposition{..})) -> do
          u <- renderUnits eventMilitaryDispositionUnits
          let (mt, tos) = case eventMilitaryDispositionType of
                         MilitaryDispositionStance i -> ("Stance", displayShowB i)
                         MilitaryDispositionFormation i -> ("Formation", displayShowB i)
          pure $ "Changed a " <> mt <> " of " <> u <> " to " <> tos
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
          pure $ "Placed a " <> renderRawObjectType eventBuildingType <> " at " <> renderPos eventBuildPos <> " with " <> u
        (EventTypeResearch (EventResearch{..})) -> do
          t <- renderObject eventResearchBuilding
          pure $ "Researched " <> displayShowB eventResearchTech <> " at " <> t
        (EventTypeTrain (EventTrain{..})) -> do
          t <- renderObject eventTrainBuilding
          pure $ "Trained " <> displayShowB eventTrainNumber <> " " <> renderRawObjectType eventTrainType  <> "s at " <> t
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
          pure $ "Deleted a " <> t
        (EventTypeResign (EventResign{..})) -> pure $ "Resigned "
        (EventTypeAttackGround (EventAttackGround{..})) -> do
          u <- renderUnits eventAttackGroundUnitIds
          pure $ "Attacked the ground at " <> renderPos eventAttackGroundPos <> " with " <> u
        (EventTypeTribute (EventTribute{..})) -> do
          toPlayer <- renderPlayer $ Just eventTributeTo
          pure $ "Tributed " <> toPlayer <> " " <> (displayShowB eventTributeAmount) <> " " <> displayShowB eventTributeResourceKind <> " with a transaction fee of " <> displayShowB eventTributeTransationFee
        (EventTypeRepair (EventRepair{..})) -> do
          u <- renderUnits eventRepairRepairers
          o <- renderObject eventRepairRepaired
          pure $ "Repaired a " <> o <> " with " <> u
        (EventTypeUngarrison (EventUngarrison{..})) -> do
          o <- renderObjects eventUngarrisonReleasedFrom
          pure $ "Ungarrisoned from " <> o
        (EventTypeToggleGate (EventToggleGate{..})) -> do
          o <- renderObject eventToggleGateGate
          pure $ "Toggled a " <> o
        (EventTypeGarrison (EventGarrison{..})) -> do
          t <- renderObject eventGarrisonTargetId
          u <- renderUnits eventGarrisonGarrisonedUnits
          pure $ "Garrisoned " <> u <> " in a " <> t
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
        (EventTypeDeath (EventDeath{..})) -> do
          o <- renderObject eventDeathObject
          lastA <- lookupEventOrFail  eventDeathFinalAction
          killedBy <- lookupEventOrFail  eventDeathKilledByEvent
          lastAR <- renderEvent lastA
          killedByR <- renderEvent killedBy
          pure $ "*** DEATH *** " <> o <>
            "\n          Last action: " <> lastAR <>
            "\n          Killed by: " <> killedByR


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

type RenderPlayer = Bool
type RenderObjectId = Bool

renderObject :: (ToObjectId a) => a -> Sim TL.Builder
renderObject o = renderObjectWithOptions o True True

renderObjectWithOptions :: (ToObjectId a) => a -> Bool -> Bool -> Sim TL.Builder
renderObjectWithOptions oid doRenderPlayer doRenderObjectId  = do
  m <- lookupObject oid
  case m of
    Nothing -> pure $ "OBJECT NOT FOUND (" <> (displayShowB . objectIdToInt . toObjectId $ oid) <> ")"
    Just o -> do
      let t = renderRestrictType $ getObjectRestrict o
      p <- if doRenderPlayer
             then do
              pr <- renderPlayer $ objectPlayer o
              pure $ " belonging to " <> pr
             else pure ""
      pure $ t <> (if doRenderObjectId then renderObjectId oid  else "" ) <> p

renderObjectId :: (ToObjectId a ) => a -> TL.Builder
renderObjectId a = " (" <> (displayShowB . objectIdToInt $ toObjectId a) <> ") "

renderUnits :: (ToObjectId a) => [a] -> Sim TL.Builder
renderUnits = renderObjects

renderObjects :: (ToObjectId a) => [a] -> Sim TL.Builder
renderObjects [] = pure "NO OBJECTS"
renderObjects is = do
  us <- mapM (\i -> renderObjectWithOptions i False True) is
  let ts = map TL.toLazyText us
      types = L.sort . L.nub $ ts
      rs = map (\t -> (displayShowTL $ length (filter ((==) t) ts)) <> " " <> t <> "s") types
  pure $ F.Buildable.build $ TL.intercalate ", " rs

renderRawObjectType :: ObjectType -> TL.Builder
renderRawObjectType = F.Buildable.build . objectTypeToText

objectTypeToText :: ObjectType -> Text
objectTypeToText ot =  T.drop 3 $ displayShowT ot

restrictionToText :: OTRestriction -> Text
restrictionToText ot =  T.drop 13 $ displayShowT ot

objectTypeWToText :: ObjectTypeW -> Text
objectTypeWToText ot =  T.drop 11 $ displayShowT ot

eventTypeWToText :: EventTypeW -> Text
eventTypeWToText ot =  T.drop 10 $ displayShowT ot

renderObjectTypes :: [ObjectType] -> TL.Builder
renderObjectTypes ots = F.Buildable.build $ T.intercalate "|" $ map objectTypeToText ots

renderMany :: [Text] -> TL.Builder
renderMany ts = F.Buildable.build $ T.intercalate "|" $ ts

renderObjectTypeW :: ObjectTypeW -> TL.Builder
renderObjectTypeW = F.Buildable.build . objectTypeWToText

renderRestrictType :: OTRestrict -> TL.Builder
renderRestrictType (OTRestrictKnown o) = renderRawObjectType o
renderRestrictType (OTRestrictOneOf o) = renderObjectTypes $ NE.toList o
renderRestrictType (OTRestrictGeneral o) = "Restricted: " <>  (F.Buildable.build $ T.intercalate "|" $ map restrictionToText  $ NE.toList o)
renderRestrictType OTRestrictNone = "UnknownType"

displayShowB :: (Show a) => a -> TL.Builder
displayShowB = F.Buildable.build . displayShowT

displayShowTL ::  Show a => a -> TL.Text
displayShowTL = utf8BuilderToLazyText . displayShow