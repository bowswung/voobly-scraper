{-# OPTIONS -fno-warn-deprecations #-}
module Data.Mgz.Deserialise where

import RIO
import qualified RIO.Text as T
import qualified RIO.List as L
import qualified Data.List as L.Partial
import Control.Monad (replicateM)
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.Combinator as AP
import qualified Data.Binary.Get as G
-- import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Compression.Zlib.Raw as Zlib
-- Partial deserialiser

import qualified Data.ByteString.Builder as Builder
import Data.Text.Encoding
import qualified Data.ByteString.Base16 as Base16
--

parseRec :: HasLogFunc env => RIO env (Either String RecInfo)
parseRec = do
  logInfo "Starting parse"
  bs <- BS.readFile "/code/recanalyst/test/recs/versions/up1.4.mgz"
  pure $ AP.parseOnly gameParser bs

data RecInfo = RecInfo {
  recInfoHeader :: Header,
  recInfoOps ::  [Op]
  }
data Header = Header {

    headerPlayers :: [PlayerInfo]
  , headerTiles :: [Tile]
  } deriving (Show)

data Pos = Pos {
  posX :: Float,
  posY :: Float
} deriving (Show)


data PosSimple = PosSimple {
  posSimpleX :: Int,
  posSimpleY :: Int
} deriving (Show)

data Tile = Tile {
  tilePositionX :: Int,
  tilePositionY :: Int,
  tileTerrain :: Int,
  tileElevation :: Int
} deriving (Show)

data PlayerInfo = PlayerInfo {
  playerInfoNumber :: Int ,
  playerInfoName :: Text,
  playerInfoObjects :: [Object]
} deriving (Show)


data Object = Object {
  objectType :: Int,
  objectOwner :: Int,
  objectUnitId :: Int,
  objectPosX :: Maybe Float,
  objectPosY :: Maybe Float,
  objectExtra :: Maybe ObjectExtra
} deriving (Show)

data ObjectExtra = ObjectExtraRes {
  objectExtraResType :: Int,
  objectExtraResAmount :: Float
} deriving (Show)

data OpSync = OpSync {
  opSyncTime :: Int
} deriving (Show)


data Command =
    CommandTypePrimary CommandPrimary
  | CommandTypeMove CommandMove
  | CommandTypeStance CommandStance
  | CommandTypeGuard CommandGuard
  | CommandTypeFollow CommandFollow
  | CommandTypePatrol CommandPatrol
  | CommandTypeFormation CommandFormation
  | CommandTypeResearch CommandResearch
  | CommandTypeBuild CommandBuild
  | CommandTypeWall CommandWall
  | CommandTypeTrain CommandTrain
  | CommandUnparsed Int ByteString
  deriving (Show)


commandType :: Command -> Text
commandType (CommandTypePrimary _) = "Primary"
commandType (CommandTypeMove _) = "Move"
commandType (CommandTypeStance _) = "Stance"
commandType (CommandTypeGuard _) = "Guard"
commandType (CommandTypeFollow _) = "Follow"
commandType (CommandTypePatrol _) = "Patrol"
commandType (CommandTypeFormation _) = "Formation"
commandType (CommandTypeResearch _) = "Research"
commandType (CommandTypeBuild _) = "Build"
commandType (CommandTypeWall _) = "Wall"
commandType (CommandTypeTrain _) = "Train"
commandType (CommandUnparsed n _) = "Unparsed: " <> displayShowT n

data CommandPrimary = CommandPrimary {
  commandPrimaryPlayerId :: Int
, commandPrimaryTargetId :: Int
, commandPrimaryPos :: Pos
, commandPrimaryUnitIds :: [Int]
} deriving (Show)

data CommandMove = CommandMove {
  commandMovePlayerId :: Int
, commandMovePos :: Pos
, commandMoveUnitIds :: [Int]
} deriving (Show)

data CommandStance = CommandStance {
  commandStanceStance :: Int
, commandStanceUnitIds :: [Int]
} deriving (Show)

data CommandGuard = CommandGuard {
  commandGuardGuarded :: Int
, commandGuardUnitIds :: [Int]
} deriving (Show)

data CommandFollow = CommandFollow {
  commandFollowFollowed :: Int
, commandFollowUnitIds :: [Int]
} deriving (Show)

data CommandPatrol = CommandPatrol {
  commandPatrolWaypoints :: [Pos]
, commandPatrolUnitIds :: [Int]
} deriving (Show)

data CommandFormation = CommandFormation {
  commandFormationPlayerId :: Int
, commandFormationFormation :: Int
, commandFormationUnitIds :: [Int]
} deriving (Show)

data CommandResearch = CommandResearch {
  commandResearchBuildingId :: Int
, commandResearchPlayerId :: Int
, commandResearchResearch :: Int
} deriving (Show)

data CommandBuild = CommandBuild {
  commandBuildPlayerId :: Int
, commandBuildPos :: Pos
, commandBuildBuildingType :: Int
, commandBuildBuilders :: [Int]
} deriving (Show)

data CommandWall = CommandWall {
  commandWallPlayerId :: Int
, commandWallStartPos :: PosSimple
, commandWallEndPos :: PosSimple
, commandWallBuildingType :: Int
, commandWallBuilders :: [Int]
} deriving (Show)

data CommandTrain = CommandTrain {
  commandTrainBuildingId :: Int
, commandTrainUnitType :: Int
, commandTrainNumber :: Int
} deriving (Show)

data Op =
    OpTypeSync OpSync
  | OpTypeCommand Command
  | OpTypeMetaGameStart
  | OpTypeMetaChat Text
  | OpTypeUnhandled Int
 deriving (Show)

opCommand :: Op -> Maybe Command
opCommand (OpTypeCommand c) = Just c
opCommand _ = Nothing

gameParser :: AP.Parser RecInfo
gameParser = do
  header <- parseHeader
  ops <- parseBody
  -- void $ mapM prettyCommand $ ops
  let _cmds = catMaybes $ map opCommand ops
      _cmdTypes = L.nub $ map commandType _cmds
  -- traceM $ displayShowT (L.sort cmdTypes)
  pure $ RecInfo header ops


prettyCommand :: Op -> AP.Parser ()
prettyCommand (OpTypeCommand c) =
  case c of
    CommandTypePrimary CommandPrimary{..} -> do
      traceM $ "Player " <> displayShowT commandPrimaryPlayerId <> " primaried " <> displayShowT commandPrimaryTargetId <> " with " <> displayShowT (length commandPrimaryUnitIds) <> " units"

    CommandTypeMove CommandMove{..} -> do
      traceM $ "Player " <> displayShowT commandMovePlayerId <> " moved to  " <> displayShowT (posX commandMovePos, posY commandMovePos) <> " with " <> displayShowT (length commandMoveUnitIds) <> " units"
    CommandUnparsed _ _-> pure ()
    _ -> traceM $ displayShowT $ commandType c

prettyCommand _ = pure ()

parseBody :: AP.Parser [Op]
parseBody = do
  AP.manyTill' parseOp AP.endOfInput


parseOp :: AP.Parser Op
parseOp = do
  opType <- parseInt32
  case opType of
    1 -> do

      l <- parseInt32
      command <- parseInt8
      commandRaw <- AP.take $ l - 1
      case AP.parseOnly (parseCommand command) commandRaw of
        Left err -> fail err
        Right c -> pure $ OpTypeCommand c
    2 -> do
      t <- parseInt32
      u <- parseInt32
      when (u == 0) $ skipN 28
      skipN 12
      pure $ OpTypeSync (OpSync t)
    4 -> do
      command <- parseInt32
      case command of
        -1 -> do
          l <- parseInt32
          c <- takeText l
          pure $ OpTypeMetaChat c
        500 -> do

          skipN 20
          pure OpTypeMetaGameStart

        _ -> fail $ "unhandled meta command: " ++ show command
    n | n > 1000 -> pure $ OpTypeUnhandled opType
      | otherwise -> fail $  "unhandled opType: " ++ show opType




parseCommand :: Int -> AP.Parser Command
parseCommand 0 = do
  commandPrimaryPlayerId <- parseInt8
  skipN 2
  commandPrimaryTargetId <- parseInt32
  selectCount <- parseInt8
  skipN 3
  commandPrimaryPos <- parsePos
  commandPrimaryUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypePrimary $ CommandPrimary{..}
parseCommand 3 = do
  commandMovePlayerId <- parseInt8
  skipN 6
  selectCount <- parseInt32
  commandMovePos <- parsePos
  commandMoveUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypeMove $ CommandMove{..}
parseCommand 18 = do
  selectCount <- parseInt8
  commandStanceStance <- parseInt8
  commandStanceUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypeStance $ CommandStance{..}
parseCommand 19 = do
  selectCount <- parseInt8
  skipN 2
  commandGuardGuarded <- parseInt32
  commandGuardUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypeGuard $ CommandGuard{..}
parseCommand 20 = do
  selectCount <- parseInt8
  skipN 2
  commandFollowFollowed <- parseInt32
  commandFollowUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypeFollow $ CommandFollow{..}
parseCommand 21 = do
  selectCount <- parseInt8
  waypointCount <- parseInt8
  skipN 1
  commandPatrolWaypoints <- parseMultiplePos waypointCount
  commandPatrolUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypePatrol $ CommandPatrol{..}
parseCommand 22 = do
  selectCount <- parseInt8
  commandFormationPlayerId <- parseInt32
  skipN 1
  commandFormationFormation <- parseInt8
  skipN 3
  commandFormationUnitIds <- parseSelectedUnits selectCount
  pure . CommandTypeFormation $ CommandFormation{..}

--research
parseCommand 101 = do
  skipN 3
  commandResearchBuildingId <- parseInt32
  commandResearchPlayerId <- parseInt8
  skipN 1
  commandResearchResearch <- parseInt16
  skipN 4
  pure . CommandTypeResearch $ CommandResearch{..}

-- build
parseCommand 102 = do
  selectCount <- parseInt8
  commandBuildPlayerId <- parseInt8
  skipN 1
  commandBuildPos <- parsePos
  commandBuildBuildingType <- parseInt16
  skipN 10
  commandBuildBuilders <- parseSelectedUnits selectCount
  pure . CommandTypeBuild $ CommandBuild{..}
--wall
parseCommand 105 = do
  selectCount <- parseInt8
  commandWallPlayerId <- parseInt8
  commandWallStartPos <- PosSimple <$> parseInt8 <*> parseInt8
  commandWallEndPos <- PosSimple <$> parseInt8 <*> parseInt8
  skipN 1
  commandWallBuildingType <- parseInt16
  skipN 6
  commandWallBuilders <- parseSelectedUnits selectCount
  pure . CommandTypeWall $ CommandWall{..}


-- train
parseCommand 119 = do
  skipN 3
  commandTrainBuildingId <- parseInt32
  commandTrainUnitType <- parseInt16
  commandTrainNumber <- parseInt16
  pure . CommandTypeTrain $ CommandTrain{..}

parseCommand n = CommandUnparsed n <$> AP.takeByteString

parseSelectedUnits :: Int -> AP.Parser [Int]
parseSelectedUnits 255 = pure []
parseSelectedUnits n = replicateM n parseInt32

parsePos :: AP.Parser Pos
parsePos = do
  x <- parseFloat 4
  y <- parseFloat 4
  pure $ Pos x y

parseMultiplePos :: Int -> AP.Parser [Pos]
parseMultiplePos n = do
  xs <- replicateM 10 $ parseFloat 4
  ys <- replicateM 10 $ parseFloat 4
  pure $ map (\(x,y)->Pos x y) $ L.Partial.take n $  zip xs ys

parseHeader :: AP.Parser Header
parseHeader = do
  headerLen <- parseInt32
  skipN 4
  headerRaw <- AP.take (headerLen - 8)
  let headerInflated = Zlib.decompress (BL.fromStrict headerRaw)
  case AP.eitherResult $ AP.parse parseInflatedHeader (BL.toStrict headerInflated) of
    Left err -> fail err
    Right h -> pure h

ignore :: a -> b -> Maybe c
ignore _ _ = Nothing

parseInflatedHeader :: AP.Parser Header
parseInflatedHeader = do
  _headerVersion <- takeText 8
  _bs <- parseFloat 4
  _includeAi <- parseInt32
  skipN 4
  _gameSpeed <-  parseInt32
  skipN 37

  _pov <-  parseInt16
  numPlayers <-  parseInt8 -- this includes gaia
  _gameMode <-  parseInt16
  skipN 60
  mapSizeX <-  parseInt32
  mapSizeY <-  parseInt32
  _zones <- parseInt32
  _allVisible <- parseBool
  _fogOfWar <-  parseBool
  tiles <- sequence $ map parseTile $ [(x,y) | y <- [0 .. mapSizeY -1], x <- [0.. mapSizeX-1]]
  obstructions <-  parseInt32
  skipN $ 4 + obstructions * 4
  void $ replicateM obstructions $ parseInt32 >>= (skipN . ((*) 8))

  mapSizeX2 <-  parseInt32
  mapSizeY2 <-  parseInt32
  skipN $ mapSizeX2 * mapSizeY2 * 4 -- skip visibility mapSizeX2
  skipN 4
  skipN =<< fmap (* 27) parseInt32
  skipN 4
  players <- sequence $ map (parsePlayerInfo numPlayers) [0..numPlayers-1]
  void $ mapM prettyPlayer players
  pure $ Header players tiles
  --pure Header{..}

prettyPlayer :: PlayerInfo -> AP.Parser ()
prettyPlayer PlayerInfo{..} = do
  traceM $ "Player: " <> displayShowT playerInfoNumber <> " called " <> displayShowT playerInfoName
  traceM $ "Total objects: " <> displayShowT (length playerInfoObjects)
  traceM $ "Map objects: " <> displayShowT (length $ filter ((==) 10 . objectType) playerInfoObjects)
  traceM $ "Creatables: " <> displayShowT (length $ filter ((==) 70 . objectType) playerInfoObjects)
  traceM $ "Buildings: " <> displayShowT (length $ filter ((==) 80 . objectType) playerInfoObjects)

parsePlayerInfo :: Int -> Int -> AP.Parser PlayerInfo
parsePlayerInfo numPlayers playerNumber = do


  skipN $ numPlayers + 43
  playerInfoName <- parseString
  skipToBreak existObjectsBreak
  objs <- AP.manyTill parseObjectAndSeparator (AP.string playerInfoEndBreak)
  --diplomacies <- traceParse $ replicateM 9
  pure $ PlayerInfo playerNumber playerInfoName objs

parseObjectAndSeparator :: AP.Parser Object
parseObjectAndSeparator = do
  o <- parseObject
  AP.skipMany $ AP.string gaiaMidObjectBreak <|> AP.string playerMidObjectBreaK
  pure o
parseObject :: AP.Parser Object
parseObject = do
  objType <-  parseInt8
  objOwner <-  parseInt8
  objUnitId <-  parseInt16
  let obj = Object objType objOwner objUnitId
  case objType of
    10 -> do
      skipN 19
      posX <-  parseFloat 4
      posY <- parseFloat 4
      skipN 12
      --ns <- traceParse "Unknown" $ replicateM 10 parseInt16
      resType <-  parseInt16
      resAmount <- parseFloat 4

      skipN 14
      pure $ obj (Just posX) (Just posY) (Just $ ObjectExtraRes resType resAmount)

    30 -> skipN 200 >> (pure  $ (obj Nothing Nothing Nothing))
    70 -> do
      skipN 19
      posX <- parseFloat 4
      posY <-  parseFloat 4
      skipToBreak specificObjectBreak
      pure  $ obj (Just posX) (Just posY) Nothing
    80 -> do
      skipN 19
      posX <- parseFloat 4
      posY <-  parseFloat 4
      skipToBreak specificObjectBreak
      skipN 127
      pure  $ obj (Just posX) (Just posY) Nothing
    _ -> fail $ "Unknown object type " ++ show objType

existObjectsBreak :: ByteString
existObjectsBreak = fst $ Base16.decode "0b0008000000020000"

playerInfoEndBreak :: ByteString
playerInfoEndBreak = fst $ Base16.decode "000b0002000000020000000b"

specificObjectBreak :: ByteString
specificObjectBreak = fst $ Base16.decode "ffffffff000080bf000080bfffffffffffffffffffffffff000000000000"

playerMidObjectBreaK  :: ByteString
playerMidObjectBreaK = fst $ Base16.decode "000b4000000080000000"

gaiaMidObjectBreak :: ByteString
gaiaMidObjectBreak = fst $ Base16.decode "000b0040000000200000"

parseTile :: (Int, Int) -> AP.Parser Tile
parseTile (x,y) = do
  terrain <- parseInt8
  elevation <- parseInt8
  pure $ Tile x y terrain elevation

displayShowT ::  Show a => a -> Text
displayShowT = utf8BuilderToText . displayShow

traceParse :: (Show a) => Text -> AP.Parser a -> AP.Parser a
traceParse msg p = do
  a <- p
  traceM $ msg <> ": " <> displayShowT a
  pure a

skipToBreak :: ByteString -> AP.Parser ()
skipToBreak br = do
  void $ AP.manyTill AP.anyWord8 (AP.string br)

takeText :: Int -> AP.Parser Text
takeText n = fmap ((T.dropAround (== '\0')) . decodeLatin1) $ AP.take n

skipN :: Int -> AP.Parser ()
skipN = void . AP.take

parseString :: AP.Parser Text
parseString = do
  l <- parseInt16
  takeText l

parseFloat :: Int -> AP.Parser Float
parseFloat n = do
  t <- AP.take n
  pure $ (G.runGet G.getFloatle (BL.fromStrict t))

parseBool :: AP.Parser Bool
parseBool = do
  t <- parseInt8
  pure $ t > 0

parseInt8 :: AP.Parser Int
parseInt8 = do
  t <- AP.take 1
  pure . fromIntegral $ (G.runGet G.getInt8 (BL.fromStrict t))

parseInt16 :: AP.Parser Int
parseInt16 = do
  t <- AP.take 2
  pure . fromIntegral $ (G.runGet G.getInt16le (BL.fromStrict t))


parseInt32 :: AP.Parser Int
parseInt32 = do
  t <- AP.take 4
  pure . fromIntegral $ (G.runGet G.getInt32le (BL.fromStrict t))

showNextNBytes :: Int -> AP.Parser ()
showNextNBytes n = do
  t <- AP.lookAhead (AP.take n)
  traceShowBytesM t
traceShowBytesM :: (Monad m) => ByteString -> m ()
traceShowBytesM bs = do
  let bs' = BL.toStrict .  Builder.toLazyByteString . Builder.byteStringHex $ bs
      t = T.intercalate " " $ T.chunksOf 2 (decodeLatin1 bs')
  traceShowM t
