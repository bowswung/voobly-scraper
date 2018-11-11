{-# OPTIONS -fno-warn-deprecations #-}

module Data.Mgz.Simulate where

import RIO

import Data.Mgz.Deserialise
import Data.Mgz.Constants
import qualified RIO.List as L
--import qualified Data.List.NonEmpty as NE
import qualified Data.IxSet.Typed as IxSet
import Control.Monad.State.Strict
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified RIO.HashMap as HM
import Data.Proxy(Proxy(..))
import Data.Maybe (fromJust)


import Data.Mgz.Simulate.Objects
import Data.Mgz.Simulate.State
import Data.Mgz.Simulate.Events
import Data.Mgz.Simulate.Render
import Data.Mgz.Simulate.Command
import Data.Mgz.Utils



replay :: GameState -> RIO LogFunc ()
replay gs = do
  logInfo "Rendering to file"
  r <- evalStateT renderEvents (SimState 0 gs HM.empty)
  liftIO $ TL.writeFile "/code/voobly-scraper/simHistory" $  r
  r2 <- evalStateT renderAllObjects (SimState 0 gs HM.empty)
  liftIO $ TL.writeFile "/code/voobly-scraper/simObjects" $  r2
  logInfo "Render done"


simulate :: RecInfo -> RIO LogFunc GameState
simulate RecInfo{..} = do
  logInfo "Start simulating"

  let initialGS  = emptyGameState recInfoHeader
      initialSS = SimState 0 initialGS HM.empty
  fmap gameState $
    (flip execStateT) initialSS $ do
      logInfo "Recreating initial game state"
      initialiseGameState recInfoHeader

      logInfo "Building base events"
      void $ mapM buildBasicEvents recInfoOps
      totalEventsSize <- fmap IxSet.size getEventSet
      logInfo $ "Total events added: " <> displayShow totalEventsSize

      logInfo $ "Linking buildings based on position"
      linkBuildingsBasedOnPosition

      logInfo "Assigning object ids to events"
      assignObjectIdsToEvents

      logInfo "Making simple inferences"
      makeSimpleInferences

      logInfo "Linking build commands with buildings"
      linkBuildingsToCommands -- this might give more info if repeated -- I'm not 100% sure

      logInfo "Assigning object ids to events"
      assignObjectIdsToEvents

      logInfo "Linking train commands with units"
      void $ replicateM 5 (linkUnitsToTrainCommands False)

      logInfo "Guessing at remaining unit types"
      linkUnitsToTrainCommands True

      objectsNotPlaced <- fmap (IxSet.getEQ (ObjectPlacedByGameIdx False)) getObjectSet
      logInfo $ displayShow (IxSet.size objectsNotPlaced) <>  " objects were found"
      logInfo $ displayShow (IxSet.size $ IxSet.getGT (Nothing :: Maybe PlayerId) $ objectsNotPlaced) <>  " were assigned to a player"
      logInfo $ displayShow (IxSet.size $ IxSet.getEQ OTRestrictWKnown $ objectsNotPlaced) <>  " were assigned a known type"
      logInfo $ displayShow (IxSet.size $ IxSet.getEQ OTRestrictWOneOf $ objectsNotPlaced) <>  " were assigned a range of known types"
      logInfo $ displayShow (IxSet.size $ IxSet.getEQ OTRestrictWGeneral $ objectsNotPlaced) <>  " were restricted by their event involvement"
      logInfo $ displayShow (IxSet.size $ IxSet.getEQ OTRestrictWNone $ objectsNotPlaced) <>  " could not have anything inferred about them"
      logInfo $ "Of which: " <> displayShow (IxSet.size $ IxSet.getEQ (ObjectWasDeletedIdx True) . IxSet.getEQ OTRestrictWNone $ objectsNotPlaced) <>  " were deleted by players"







initialiseGameState :: Header -> Sim ()
initialiseGameState h = do
  void $ (flip mapM) (headerPlayers h) $ \PlayerInfo{..} -> do
    mapM handlePlayerObject $ reclassifyExtraneousObjectParts playerInfoObjects
    where
      -- we are reassigning these for now
      reclassifyExtraneousObjectParts :: [ObjectRaw] -> [ObjectRaw]
      reclassifyExtraneousObjectParts [] = []
      reclassifyExtraneousObjectParts [x] = [x]
      reclassifyExtraneousObjectParts ((!x):(!xs)) =
        let (toRe, rest) = L.splitAt (objectPartsNumber (objectRawUnitId x) - 1) xs
        in x : (map (\o -> o{objectRawUnitId = OT_ExtraneousPart (objectRawUnitId o)}) toRe) ++ rest


buildBasicEvents :: Op -> Sim ()
buildBasicEvents (OpTypeSync OpSync{..}) = modify' (\ss -> ss{ticks = ticks ss + opSyncTime})
buildBasicEvents (OpTypeCommand cmd) = handleCommand cmd
buildBasicEvents _ = pure ()

handlePlayerObject :: ObjectRaw -> Sim ()
handlePlayerObject oRaw@ObjectRaw{..} = do
  let (o, mObject) = objectFromObjectRaw oRaw
  placeMapObject mObject objectRawPos
  void $ updateObject o
    where
      --debugObjectRaw :: Sim ()
      --debugObjectRaw = traceM $ displayShowT objectRawObjectId <> " " <> displayShowT objectRawOwner  <> " " <> (displayShowT $ normaliseObjectType objectRawUnitId) <> " at " <> displayShowT objectRawPos

placeMapObject :: MapObject -> Pos -> Sim ()
placeMapObject mo p = do
  mTiles <- fmap (mapTiles . gameState) get
  let mTile = IxSet.getOne $ IxSet.getEQ (posToCombinedIdx p) mTiles
  case mTile of
    Nothing -> error $ "Could not find map tile at " ++ show p ++ " for object " ++ show mo
    Just t -> do
      let newOs = mapTileObjects t ++ [mo]
      if length (filter (not . canOverlap) newOs) < 2
        then do
          let newMap = IxSet.updateIx (mapTileCombinedIdx t) t{mapTileObjects = newOs} mTiles
          modify' $ \ss ->
            let gs = gameState ss
            in ss{gameState = gs{mapTiles = newMap}}
        else  error $ "Overlap in map tile when trying to place " ++ show mo ++ " at " ++ show p ++ ": found " ++ show (mapTileObjects t)
    where
      canOverlap :: MapObject -> Bool
      canOverlap = canObjectTypeOverlap . objectRawUnitId . mapObjectOriginal


assignObjectIdsToEvents :: Sim ()
assignObjectIdsToEvents = do
  objectsForwards <- fmap (IxSet.toAscList (Proxy :: Proxy ObjectId) . IxSet.getEQ (ObjectPlacedByGameIdx False)) $ getObjectSet
  void $ mapM assignObjectIdToEvent objectsForwards
  where
    assignObjectIdToEvent :: Object -> Sim ()
    assignObjectIdToEvent o = do
      mustBeAfterOrAt <- fmap  (headMaybe . IxSet.toDescList (Proxy :: Proxy EventId)  . IxSet.getLT (EventObjectIdAssignmentIdx $ objectId o)) getEventSet
      case mustBeAfterOrAt of
        Just e -> void $ updateEvent $ e{eventAssignObjectIds = L.sort . L.nub $ eventAssignObjectIds e ++ [objectId o]}
        Nothing -> traceM $ "Found object id that was never referenced"



makeSimpleInferences :: Sim ()
makeSimpleInferences = do
  unknownUnits <- fmap (IxSet.getLT OTRestrictionIsMilitaryUnit . IxSet.getEQ ObjectTypeWUnit) $ getObjectSet
  void $ mapM inferUnitType $ IxSet.toList unknownUnits

  unknownBuildings <- fmap (IxSet.getGT OTRestrictWKnown . IxSet.getEQ ObjectTypeWBuilding) $ getObjectSet
  void $ mapM inferBuildingType $ IxSet.toList unknownBuildings

  unplayerEvents <- fmap (IxSet.getEQ (Nothing :: Maybe PlayerId)) $ getEventSet
  void $ mapM inferPlayerForEvent $ IxSet.toList unplayerEvents

  eventsWithPlayer <- fmap (IxSet.getGT (Nothing :: Maybe PlayerId)) $ getEventSet
  void $ mapM inferPlayerForObjects $ IxSet.toList eventsWithPlayer


  primaryEvents <- fmap (IxSet.getEQ EventTypeWPrimary) $ getEventSet
  void $ mapM inferDetailForEvent $ IxSet.toList primaryEvents

  garrisonAndRepairEvents <- fmap (ixsetGetIn [EventTypeWRepair, EventTypeWGarrison, EventTypeWUngarrison]) $ getEventSet
  void $ mapM inferPlayerEtcForRepairGarrison $ IxSet.toList garrisonAndRepairEvents

-- , OTRestrictionIsGarrisonable, OTRestrictionIsUngarrisonable

  unknownRepairables <- fmap (ixsetGetIn [OTRestrictionIsRepairable] . IxSet.getEQ ObjectTypeWUnknown) $ getObjectSet
  void $ mapM inferIsRepairableBuilding $ IxSet.toList unknownRepairables

  pure ()
  where
    inferPlayerForEvent :: Event -> Sim ()
    inferPlayerForEvent e = do
      objs <- fmap catMaybes $  mapM lookupObject $ eventActingObjectsIdx e
      let ps = L.nub . catMaybes $ map objectPlayer objs
      case ps of
        [] -> pure ()
        [x] -> void $ updateEvent e{eventPlayerResponsible = Just x}
        xs -> error $ "Multiple player owners for units in single event" ++ show xs

    inferPlayerForObjects :: Event -> Sim ()
    inferPlayerForObjects e@Event{..} = do
      case eventPlayerResponsible of
        Nothing -> pure ()
        Just pid -> do
          objs <- fmap catMaybes $  mapM lookupObject $ eventActingObjectsIdx e
          void $ mapM ((flip updateObjectWithPlayerIfNone) pid) objs
    inferPlayerEtcForRepairGarrison :: Event -> Sim ()
    inferPlayerEtcForRepairGarrison Event{..} =
      case eventType of
        EventTypeRepair EventRepair{..} -> do
          o <- lookupObjectOrFail eventRepairRepaired
          o' <- updateWithRestriction o OTRestrictionIsRepairable
          void $ updateObjectWithMaybePlayerIfNone o' eventPlayerResponsible
        EventTypeGarrison EventGarrison{..} -> do
          o <- lookupObjectOrFail eventGarrisonTargetId
          o' <- updateWithRestriction o OTRestrictionIsGarrisonable
          void $ updateObjectWithMaybePlayerIfNone o' eventPlayerResponsible
        EventTypeUngarrison EventUngarrison{..} -> do
          os <- mapM lookupObjectOrFail eventUngarrisonReleasedFrom
          void $ (flip mapM) os $ \o -> do
            o' <- updateWithRestriction o OTRestrictionIsUngarrisonable
            updateObjectWithMaybePlayerIfNone o' eventPlayerResponsible

        _ -> pure ()

    inferUnitType :: Object -> Sim ()
    inferUnitType o = do
      villagerEvents <- fmap (ixsetGetIn [EventTypeWBuild, EventTypeWRepair]  . IxSet.getEQ (objectId o)) $ getEventSet
      if IxSet.size villagerEvents > 0
        then void $ updateWithObjectType o OT_Villager
        else do
          militaryEvents <- fmap (ixsetGetIn [EventTypeWPatrol, EventTypeWMilitaryDisposition, EventTypeWTargetedMilitaryOrder]  . IxSet.getEQ (objectId o)) $ getEventSet
          if IxSet.size militaryEvents > 0
            then void $ updateWithRestriction o OTRestrictionIsMilitaryUnit
            else pure ()

    inferBuildingType :: Object -> Sim ()
    inferBuildingType o = do
      techEvents <-  fmap (IxSet.toList . ixsetGetIn [EventTypeWResearch]  . IxSet.getEQ (objectId o)) $ getEventSet
      let researchedTechs = L.nub . catMaybes $ map eventTechType techEvents
          utsFromTechs = L.nub . concat . catMaybes $ map ((flip HM.lookup) techToBuildingMap) researchedTechs
      when (length utsFromTechs < 1 && length techEvents > 0) $ traceM $ "Could not find a building type that researches " <> displayShowT researchedTechs

      trainEvents <- fmap (IxSet.toList . ixsetGetIn [EventTypeWTrain]  . IxSet.getEQ (objectId o)) $ getEventSet
      let trainedUnitTypes = L.nub . catMaybes $ map eventTrainObjectType trainEvents
          utsFromTrainedUnits = L.nub . concat . catMaybes $ map ((flip HM.lookup) trainUnitToBuildingMap) trainedUnitTypes
      when (length utsFromTrainedUnits < 1 && length trainEvents > 0) $ traceM $ "Could not find a building type that trains " <> displayShowT trainedUnitTypes

      case L.nub $ concat [utsFromTechs, utsFromTrainedUnits] of
        [] -> pure ()
        [x] -> void $ updateWithObjectType o x
        xs -> void $ updateWithObjectTypes o $ nonEmptyPartial xs

    inferIsRepairableBuilding :: Object -> Sim ()
    inferIsRepairableBuilding o = do
      allEventsPrior <- eventsPriorToObjectCreation o
      let eventsToLookAt = case objectPlayer o of
                            Nothing -> allEventsPrior
                            Just pid -> IxSet.getEQ (Just pid) allEventsPrior
      -- there were no train events that could have created this repairable so it must be a building
      if IxSet.null $ ixsetGetIn (map (Just . EventTrainedObjectTypeIdx) repairableUnits) eventsToLookAt
        then void $ updateWithRestriction o OTRestrictionIsBuilding
        else pure ()

    inferDetailForEvent :: Event -> Sim ()
    inferDetailForEvent e@Event{..} = do
      mEt <-
        case eventType of
          EventTypePrimary EventPrimary{..} -> do
            target <- lookupObjectOrFail eventPrimaryTarget
            actors <- mapM lookupObjectOrFail eventPrimaryObjects
            tryWhileNothing $ map (\f -> f eventPlayerResponsible target actors eventPrimaryPos) [construeAsGather, construeAsAttack, construeAsRelicGather, construeAsVillOnRepairable]
          _ -> pure Nothing
      case mEt of
            Nothing -> pure ()
            Just et -> void $ updateEvent (e{eventType = et})

    construeAsGather :: Maybe PlayerId -> Object -> [Object] -> Pos -> Sim (Maybe EventType)
    construeAsGather pId t actors p =
       -- herdables can show up in these events
      if and (map isVillager $ filter (not . isHerdable) actors) && isResource t && (not $ isObjectEnemy pId t)
        then do
         pure . Just $ EventTypeGather EventGather{
            eventGatherTargetId = objectId t
          , eventGatherGatherers = map (unitId . asUnit) actors
          , eventGatherPos = p
          }
        else
          if and (map ((==) ObjectTypeWUnit . objectTypeW) actors) && isResource t &&  (not $ isObjectEnemy pId t) && (anyMeetsRestriction t OTRestrictionIsNotActableByMilitary)
            then do
              actors' <- mapM (\o -> updateObject $  restrictObjectType o OTRestrictionIsLandResourceGatherer) actors
              pure . Just $ EventTypeGather EventGather{
                    eventGatherTargetId = objectId t
                  , eventGatherGatherers = map (unitId . asUnit) actors'
                  , eventGatherPos = p
                }
            else pure Nothing

    construeAsVillOnRepairable :: Maybe PlayerId -> Object -> [Object] -> Pos -> Sim (Maybe EventType)
    construeAsVillOnRepairable pId origT actors p =
      if and (map isVillager actors) && (isObjectFriend pId origT) && (not $ isResource origT)
        then do
          -- vills can only primary act on owned objects if they are reppairables
          t <- if isBuilding origT then pure origT else updateWithRestriction origT OTRestrictionIsRepairable
          let ts =
                if isBuilding t
                  then [EventTypeWRepair, EventTypeWBuild] ++ if isNotDropoffBuilding t then [] else [EventTypeWDropoff]
                  else if isUnit t
                         then [EventTypeWRepair]
                         else [EventTypeWRepair, EventTypeWBuild, EventTypeWDropoff]

          pure . Just $ EventTypeVillOnRepairable EventVillOnRepairable{
            eventVillOnRepairableType = nonEmptyPartial ts
          , eventVillOnRepairableObject = objectId t
          , eventVillOnRepairableVills = map (unitId . asUnit) actors
          , eventVillOnRepairablePos = p
          }
        else pure Nothing




    construeAsAttack :: Maybe PlayerId -> Object -> [Object] -> Pos -> Sim (Maybe EventType)
    construeAsAttack pId t actors p =
      if isObjectEnemy pId t -- this doesn't take into account trade carts/cogs going to enemy markets/docks
        then do
         void $ (flip mapM) actors $ \b -> updateWithRestriction b OTRestrictionCanAttack

         pure . Just $ EventTypeAttack EventAttack{
                eventAttackAttackers =  map objectId actors
              , eventAttackTargetId = objectId t
              , eventAttackPos = p
              }
        else
          case pId of
            Just thisPlayer ->
              --if military units primary something unknown then it must belong to the enemy
              if or (map (\a -> isMilitaryUnit a && (not $ isMonk a)) actors) && objectPlayer t == Nothing
                then do
                  let otherPlayer = if thisPlayer == PlayerId 1 then PlayerId 2 else PlayerId 1 -- @team
                  void $ updateObjectWithPlayerIfNone t otherPlayer
                  pure Nothing
                else pure Nothing
            _ -> pure Nothing

    construeAsRelicGather :: Maybe PlayerId -> Object -> [Object] -> Pos -> Sim (Maybe EventType)
    construeAsRelicGather _pId t actors p =
      if getObjectType t == Just OT_Relic -- the only people who can primary relics are monks
        then do
         actors' <- mapM ((flip updateWithObjectType) OT_Monk) actors
         pure . Just $ EventTypeGatherRelic EventGatherRelic{
                eventGatherRelicGatherers =  map (unitId . asUnit) actors'
              , eventGatherRelicTargetId = objectId t
              , eventGatherRelicPos = p
              }
        else pure Nothing



linkBuildingsBasedOnPosition :: Sim ()
linkBuildingsBasedOnPosition = do
  unlinkedObjects <- fmap (IxSet.toList .  ixsetGetIn [ObjectTypeWUnknown, ObjectTypeWBuilding]) $ findUnlinkedObjects
  logInfo $ (displayShow $ length unlinkedObjects) <> " unlinked objects found"
  void $ mapM assignBasedOnPosition unlinkedObjects

  remainingUnknownObjects <- fmap (IxSet.toList .  IxSet.getEQ (ObjectTypeWUnknown)) $ findUnlinkedObjects
  void $ mapM classifyAsNotBuilding remainingUnknownObjects
  makeSimpleInferences
  where
    classifyAsNotBuilding :: Object -> Sim ()
    classifyAsNotBuilding o =
      case objectPosHistory o of
        [] -> pure ()
        _ -> do
          possibleEvents <- fmap (IxSet.getEQ EventTypeWWall) $ findUnconsumedBuildOrWallEventsForObject o
          if IxSet.null possibleEvents
            then void $ updateWithRestriction o OTRestrictionIsUnit -- if it wasn't placed as a building or wall, and it has a position, then it must be a unit...
            else pure ()
              -- we could probably do some pretty confident guessing at wall objects here, particularly because wall commands generate a nice sequential sequence of object ids, but we won't bother for now


    assignBasedOnPosition :: Object -> Sim ()
    assignBasedOnPosition o =
      case objectPosHistory o of
        [p] -> do
          possibleEvents <- fmap (IxSet.toList . IxSet.getEQ (Just . EventSinglePosIdx $ p ) .  IxSet.getEQ EventTypeWBuild) $ findUnconsumedBuildOrWallEventsForObject o
          case possibleEvents of
            [e] -> linkBuildingToEvent o e
            _ -> pure ()
        _ -> pure ()



linkBuildingsToCommands :: Sim ()
linkBuildingsToCommands = do
  buildingsMissingInfo <- fmap (IxSet.toList .  IxSet.getEQ (ObjectTypeWBuilding)) $ findUnlinkedObjects
  logInfo $ (displayShow $ length buildingsMissingInfo) <> " unlinked buildings found"
  void $ mapM assignBasedOnBuildOrders buildingsMissingInfo
  makeSimpleInferences

  where
    assignBasedOnBuildOrders :: Object -> Sim ()
    assignBasedOnBuildOrders o = do
      possibleEvents <- fmap IxSet.toList $ findUnconsumedBuildOrWallEventsForObject o
      foundE <- case possibleEvents of
         [] -> do
            traceM $ "\n\nImpossible - this building was never placed?"
            traceShowM $ o
            allBuildEventsPrior <- fmap (IxSet.toList . IxSet.getEQ EventTypeWBuild) $  findEventsRangeForObjectCreation o Nothing
            void $ mapM debugBuildEvent $ allBuildEventsPrior
            pure Nothing
         [x] -> pure . Just $ x
         _ -> pure Nothing
      case foundE of
        Nothing -> do
         -- we can't find the specific event, but maybe we can assign some info
         let possiblePlayers = L.nub . catMaybes $ map eventPlayerResponsible possibleEvents
             possibleTypes = L.nub . catMaybes $ map eventBuildBuildingObjectType possibleEvents

         o' <- case possiblePlayers of
                 [p] -> updateObjectWithPlayerIfNone o p
                 _ -> pure o

         case possibleTypes of
           [] -> traceM $ "No possible types found!"
           [ot] -> void $ updateObject $ setObjectType o' ot
           _ -> void $ updateWithObjectTypes o' (nonEmptyPartial possibleTypes)

         case (possiblePlayers, possibleTypes, possibleEvents) of
            ([pid], [_], e1:[e2]) -> do -- all the same player and type and exactly two events
              deleteEvents <- fmap (IxSet.toList . IxSet.getEQ EventTypeWDelete . IxSet.getEQ (Just pid)) $ eventsPriorToObjectCreation o
              case filter (\e -> eventId e > eventId e1 && eventId e < eventId e2) deleteEvents of
                [del] -> do
                  -- link the del event object with the first event
                  deletedObj <- lookupObjectOrFail $ getEventDeleteObjectId del
                  if isBuilding deletedObj || objectTypeW deletedObj == ObjectTypeWUnknown
                    then do
                      linkBuildingToEvent deletedObj e1
                      linkBuildingToEvent o e2
                    else pure ()

                [] -> -- there are no relevant delete events - we know that this building definitely came from one of these orders so we will pick the first one (in cases where buildings were destroyed this might be wrong)
                  linkBuildingToEvent o e1
                _ ->  pure () -- for now we won't do anything with this - with map info we could be sure at this point so we'll leave it for now
            _ -> pure ()
              -- same - map info would probably get most of the rest of these - we can also compare timing if there are exactly two events for two players or whatever
             -- traceM "\n\n"
             -- traceShowM o
             -- void $ mapM debugBuildEvent $ possibleEvents



        Just e ->
        -- there is only one possible build event - we can link these together
          linkBuildingToEvent o e

linkBuildingToEvent :: Object -> Event -> Sim ()
linkBuildingToEvent o e = do
  o' <- updateObject $ setObjectType o $ fromJust $ eventBuildBuildingObjectType e
  o'' <- case eventPlayerResponsible e of
           Just p -> updateObjectWithPlayerIfNone o' p
           Nothing -> pure o'

  void $ updateEvent $ setEventLinkedBuilding e (buildingId . asBuilding $ o'')
  void $ updateObject $ setBuildingPlaceEvent o'' $ Just (eventId e)
  pure ()


debugBuildEvent :: Event -> Sim ()
debugBuildEvent e = do
  tl <- renderEvent e
  traceM $ TL.toStrict $ TL.toLazyText tl













linkUnitsToTrainCommands :: Bool -> Sim ()
linkUnitsToTrainCommands forceConsumeEvents = do
  unitsMissingInfo <- fmap (IxSet.toList .
                            IxSet.getEQ (ObjectTypeWUnit)) $
                            findUnlinkedObjects
  logInfo $ (displayShow $ length unitsMissingInfo) <> " unlinked units found"
  void $ mapM assignBasedOnTrainOrders $ unitsMissingInfo
  makeSimpleInferences
  where
    assignBasedOnTrainOrders :: Object -> Sim ()
    assignBasedOnTrainOrders o = do
      possibleEvents <- findUnconsumedTrainEventsForObject o
      foundE <- case possibleEvents of
         [] -> do
            traceM $ "\n\nImpossible - this unit was never trained?"
            traceShowM $ o
            allTrainEventsPrior <- fmap (IxSet.toList . IxSet.getEQ EventTypeWTrain) $  findEventsRangeForObjectCreation o Nothing
            void $ mapM debugBuildEvent $ allTrainEventsPrior
            pure Nothing
         [x] -> pure . Just $ x
         _ -> pure Nothing
      case foundE of
        Nothing -> do
          case (forceConsumeEvents, possibleEvents) of
            (True, (e:_)) -> do
              -- we are just consuming the earliest matching event
              o' <- if (isNothing $ getObjectType o)
                then updateObject $ setObjectTypes o $ nonEmptyPartial $ L.nub $ map eventTrainUnitObjectType possibleEvents
                else pure $ o

              void $ updateEvent $ setEventConsumedWithUnit e (unitId . asUnit $ o')
              --void $ updateObject $ setUnitTrainEvent o'' $ Just (eventId e)

            (False, (e:_)) -> do
               -- we can't find the specific event, but maybe we can assign some info
               let possiblePlayers = L.nub . catMaybes $ map eventPlayerResponsible possibleEvents
                   possibleTypes = L.nub $ map eventTrainUnitObjectType possibleEvents
               o' <- case possiblePlayers of
                       [p] -> updateObjectWithPlayerIfNone o p
                       _ -> pure o

               o'' <- case possibleTypes of
                      [ot] -> updateObject $ setObjectType o' ot
                      _ -> updateWithObjectTypes o' (nonEmptyPartial possibleTypes)
               case (possiblePlayers, possibleTypes) of
                 ([_], [_]) -> do
                  -- we only have one type of train event and one player so we can just assign this to the first train event we  have (e). we can't eliminate all dangling train events but we can do fairly well with this approach it seems
                  linkUnitToEvent o'' e
                 _ -> pure ()
            _ -> pure ()
        Just e ->
        -- there is only one possible train event - we can link these together
          linkUnitToEvent o e

    linkUnitToEvent :: Object -> Event -> Sim ()
    linkUnitToEvent o e = do
      o' <- updateObject $ setObjectType o $ eventTrainUnitObjectType e
      o'' <- case eventPlayerResponsible e of
               Just p -> updateObjectWithPlayerIfNone o' p
               Nothing -> pure o'

      void $ updateEvent $ setEventLinkedUnit e (unitId . asUnit $ o'')
      void $ updateObject $ setUnitTrainEvent o'' $ Just (eventId e)
      pure ()

