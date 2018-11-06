module Voobly.Analyzer where

import RIO
import qualified RIO.Text as T
import qualified RIO.List as L
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

parseRec :: HasLogFunc env => RIO env ()
parseRec = do
  bs <- BS.readFile "/code/recanalyst/test/recs/versions/up1.4.mgz"
  case AP.parseOnly gameParser bs of
    Left err -> logError $ displayShow err
    Right h -> do
      pure ()

data Header = Header {

    headerPlayers :: [PlayerInfo]
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



data Op =
    OpTypeSync OpSync
  | OpTypeCommand Int
  | OpTypeMetaGameStart
  | OpTypeMetaChat Text
  | OpTypeUnhandled Int
 deriving (Show)

gameParser :: AP.Parser Header
gameParser = do
  header <- parseHeader
  actions <- parseBody
  pure header


parseBody :: AP.Parser [Op]
parseBody = do
  AP.manyTill' parseAction AP.endOfInput


parseAction :: AP.Parser Op
parseAction = do
  opType <- parseInt32
  case opType of
    1 -> do

      l <- traceParse "length" parseInt32
      command <- traceParse "command" parseInt8

      case command of
        _ -> do
          skipN $ l -1
          pure $ OpTypeCommand command
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
  headerVersion <- traceParse "headerVersion" $ takeText 8
  bs <- parseFloat 4
  _includeAi <- parseInt32
  skipN 4
  gameSpeed <- traceParse "gameSpeed" parseInt32
  skipN 37

  pov <- traceParse "pov"  parseInt16
  numPlayers <- traceParse "numPlayers" parseInt8 -- this includes gaia
  gameMode <- traceParse "gameMode" parseInt16
  skipN 60
  mapSizeX <- traceParse "mapSizeX" parseInt32
  mapSizeY <- traceParse "mapSizeY" parseInt32
  zones <- traceParse "zones" parseInt32
  allVisible <- traceParse "allVisible" $ parseBool
  fogOfWar <- traceParse "fogOfWar" $ parseBool
  tiles <- sequence $ map parseTile $ [(x,y) | y <- [0 .. mapSizeY -1], x <- [0.. mapSizeX-1]]
  obstructions <- traceParse "obstructions" $ parseInt32
  skipN $ 4 + obstructions * 4
  replicateM obstructions $ parseInt32 >>= (skipN . ((*) 8))

  mapSizeX2 <- traceParse "mapSizeX2" parseInt32
  mapSizeY2 <- traceParse "mapSizeY2" parseInt32
  skipN $ mapSizeX2 * mapSizeY2 * 4 -- skip visibility mapSizeX2
  skipN 4
  skipN =<< fmap (* 27) parseInt32
  somVar <- traceParse "somVar" $ parseInt32
  players <- sequence $ map (parsePlayerInfo numPlayers) [0..numPlayers-1]
  mapM prettyPlayer players
  pure $ Header players
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
  playerInfoName <- traceParse "playerInfoName" parseString
  skipToBreak existObjectsBreak
  objs <- AP.manyTill parseObjectAndSeparator (AP.string playerInfoEndBreak)
  --diplomacies <- traceParse $ replicateM 9
  pure $ PlayerInfo playerNumber playerInfoName objs

parseObjectAndSeparator :: AP.Parser Object
parseObjectAndSeparator = do
  o <- parseObject
  showNextNBytes 10
  AP.skipMany $ AP.string gaiaMidObjectBreak <|> AP.string playerMidObjectBreaK
  pure o
parseObject :: AP.Parser Object
parseObject = do
  objType <- traceParse "objType" parseInt8
  objOwner <- traceParse "objOwner" parseInt8
  objUnitId <- traceParse "objUnitId" parseInt16
  let obj = Object objType objOwner objUnitId
  case objType of
    10 -> do
      skipN 19
      posX <- traceParse "posX" $ parseFloat 4
      posY <- traceParse "posY"  $ parseFloat 4
      skipN 12
      --ns <- traceParse "Unknown" $ replicateM 10 parseInt16
      resType <- traceParse "resType"  parseInt16
      resAmount <- traceParse "resAmount"  $ parseFloat 4

      skipN 14
      pure $ obj (Just posX) (Just posY) (Just $ ObjectExtraRes resType resAmount)

    30 -> skipN 200 >> (pure  $ (obj Nothing Nothing Nothing))
    70 -> do
      skipN 19
      posX <- traceParse "posX" $ parseFloat 4
      posY <- traceParse "posY"  $ parseFloat 4
      skipToBreak specificObjectBreak
      pure  $ obj (Just posX) (Just posY) Nothing
    80 -> do
      skipN 19
      posX <- traceParse "posX" $ parseFloat 4
      posY <- traceParse "posY"  $ parseFloat 4
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
