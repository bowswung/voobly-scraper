{-# OPTIONS -fno-warn-deprecations #-}

module Data.Mgz.Deserialise.BasicTypes where

import RIO
import qualified RIO.Text as T
import qualified Data.List as L.Partial
import Control.Monad (replicateM)
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import qualified Data.Binary.Parser as G
import Data.Binary.Get (Get)

import qualified Data.ByteString.Builder as Builder
import Data.Text.Encoding
import qualified Data.ByteString.Base16 as Base16

import Data.Mgz.Constants

newtype ObjectId = ObjectId {objectIdToInt :: Int} deriving (Show, Eq, Ord, Generic) -- object id from rec file - we don't know anything about it!
instance Hashable ObjectId



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


type SGet a = Get a

class SimpleGet a where
  simpleGet :: SGet a


type EitherInheritOrIds = Either () [ObjectId]

getSelectedUnitsOrInherit :: Int -> Get EitherInheritOrIds
getSelectedUnitsOrInherit 255 = pure $ Left ()
getSelectedUnitsOrInherit (-1) = pure $ Left ()
getSelectedUnitsOrInherit 0 = fail "No units to parse in getSelectedUnitsOrInherit"
getSelectedUnitsOrInherit n | n < 0 = fail $ "Got a negative number in getSelectedUnitsOrInherit" ++ show n
                            | otherwise = fmap Right $ replicateM n (fmap ObjectId getInt32Int)

getSelectedUnits :: Int -> Get [ObjectId]
getSelectedUnits 0 = fail "No units to parse"
getSelectedUnits n | n < 0 = fail "Got a negative number in getSelectedUnits"
                     | otherwise =  replicateM n (fmap ObjectId getInt32Int)

data GarrisonType =
    GarrisonTypePack
  | GarrisonTypeUnpack
  | GarrisonTypeUnknown
  | GarrisonTypeGarrison
  deriving (Show, Eq, Ord)

getGarrisonType :: Get GarrisonType
getGarrisonType = do
  t <- getInt8Int
  case t of
    1 -> pure $ GarrisonTypePack
    2 -> pure $ GarrisonTypeUnpack
    4 -> pure $ GarrisonTypeUnknown
    5 -> pure $ GarrisonTypeGarrison
    _ -> fail $ "Could not covert int " ++ show t ++ " to a GarrisonType"

getResourceKind :: Get ResourceKind
getResourceKind = do
  k <- getInt8Int
  case k of
    0 -> pure $ ResourceKindFood
    1 -> pure $ ResourceKindWood
    2 -> pure $ ResourceKindStone
    3 -> pure $ ResourceKindGold
    _ -> fail $ "Could not covert int " ++ show k ++ " to a ResourceKind"
data Pos = Pos {
  posX :: Float,
  posY :: Float
} deriving (Show, Eq, Ord)

getPos :: Get Pos
getPos = do
  x <- G.getFloatle
  y <- G.getFloatle
  pure $ Pos x y

getMaybePos :: Get (Maybe Pos)
getMaybePos = do
  x <- G.getFloatle
  y <- G.getFloatle
  if x < 0 && y < 0
    then pure Nothing
    else pure . Just $ Pos x y

getMultiplePos :: Int -> Get [Pos]
getMultiplePos n = do
  xs <- replicateM 10 $ G.getFloatle
  ys <- replicateM 10 $ G.getFloatle
  pure $ map (\(x,y)->Pos x y) $ L.Partial.take n $  zip xs ys

data PosSimple = PosSimple {
  posSimpleX :: Int,
  posSimpleY :: Int
} deriving (Show, Eq, Ord)

data Tile = Tile {
  tilePositionX :: Int,
  tilePositionY :: Int,
  tileTerrain :: Int,
  tileElevation :: Int
} deriving (Show, Eq, Ord)

getTile :: (Int, Int) -> Get Tile
getTile (x,y) = do
  terrain <- getInt8Int
  elevation <- getInt8Int
  pure $ Tile x y terrain elevation

data PlayerInfo = PlayerInfo {
  playerInfoPlayerId :: PlayerId,
  playerInfoName :: Text,
  playerInfoObjects :: [ObjectRaw]
} deriving (Show, Eq, Ord)

getPlayerInfo :: Int -> PlayerId -> Get PlayerInfo
getPlayerInfo numPlayers playerNumber = do
  G.skip $ numPlayers + 43

  playerInfoName <- getString
  skipToBreak existObjectsBreak
  objs <- G.manyTill' getObjectRawSkipSeparators (G.string playerInfoEndBreak)
  --diplomacies <- traceParse $ replicateM 9
  pure $ PlayerInfo playerNumber playerInfoName objs

data ObjectRaw = ObjectRaw {
  objectRawType :: Int,
  objectRawOwner :: PlayerId,
  objectRawUnitId :: ObjectType,
  objectRawHitpoints :: Int,
  objectRawObjectId :: ObjectId,
  objectRawPos :: Pos,
  objectRawExtra :: Maybe ObjectRawExtra
} deriving (Show, Eq, Ord)

data ObjectRawExtra = ObjectRawExtraRes {
  objectRawExtraResType :: Int,
  objectRawExtraResAmount :: Float
} deriving (Show, Eq, Ord)

getObjectRawSkipSeparators :: Get ObjectRaw
getObjectRawSkipSeparators = do
  o <- getObjectRaw
  G.skipMany $ G.string gaiaMidObjectBreak <|> G.string playerMidObjectBreaK
  pure o

getObjectRaw :: Get ObjectRaw
getObjectRaw = do
  objType <-  getInt8Int
  objOwner <- fmap PlayerId getInt8Int
  objUnitId <- fmap normaliseObjectType getInt16Int
  G.skip 6
  objHitpoints <- getInt32Int
  G.skip 4
  objId <- fmap ObjectId  getInt32Int
  G.skip 1
  pos <- getPos
  let obj = ObjectRaw objType objOwner objUnitId objHitpoints objId pos
  case objType of
    -- resources and similar?
    10 -> do
      G.skip 12
      --ns <- traceParse "Unknown" $ replicateM 10 getInt16Int
      resType <-  getInt16Int
      resAmount <- G.getFloatle
      G.skip 14
      pure $ obj (Just $ ObjectRawExtraRes resType resAmount)

    30 -> G.skip 173 >> (pure  $ (obj Nothing))
    -- units
    70 -> do
      skipToBreak specificObjectBreak
      pure  $ obj Nothing
    -- buildings
    80 -> do
      skipToBreak specificObjectBreak
      G.skip 127
      pure  $ obj  Nothing
    _ -> fail $ "Unknown object type " ++ show objType


debugPlayer :: PlayerInfo -> Get ()
debugPlayer PlayerInfo{..} = do
  traceM $ "Player: " <> displayShowT playerInfoPlayerId <> " called " <> displayShowT playerInfoName
  traceM $ "Total objects: " <> displayShowT (length playerInfoObjects)
  traceM $ "Map objects: " <> displayShowT (length $ filter ((==) 10 . objectRawType) playerInfoObjects)
  traceM $ "Creatables: " <> displayShowT (length $ filter ((==) 70 . objectRawType) playerInfoObjects)
  traceM $ "Buildings: " <> displayShowT (length $ filter ((==) 80 . objectRawType) playerInfoObjects)


{-

Helper functions
-}






runGetEither :: Get a -> BL.ByteString -> Either String a
runGetEither p bl =
  case G.runGetOrFail p bl of
    Left(_,_,err) -> Left err
    Right(_,_,a) -> Right a


displayShowT ::  Show a => a -> Text
displayShowT = utf8BuilderToText . displayShow

traceParse :: (Show a) => Text -> Get a -> Get a
traceParse msg p = do
  a <- p
  traceM $ msg <> ": " <> displayShowT a
  pure a

-- this is not very efficient but is there another way?
skipToBreak :: ByteString -> Get ()
skipToBreak br = void $ G.manyTill' G.anyWord8 (G.string br)

takeText :: Int -> Get Text
takeText n = fmap ((T.dropAround (== '\0')) . decodeLatin1) $ G.getByteString n

getString :: Get Text
getString = do
  l <- getInt16Int
  takeText l


getBool :: Get Bool
getBool = fmap ((<) 0) G.getInt8

getInt8Int :: Get Int
getInt8Int = fmap fromIntegral G.getInt8

getInt16Int :: Get Int
getInt16Int =  fmap fromIntegral G.getInt16le

getInt32Int :: Get Int
getInt32Int = fmap fromIntegral G.getInt32le

getObjectId :: Get ObjectId
getObjectId = fmap ObjectId getInt32Int

getMaybeObjectId :: Get (Maybe ObjectId)
getMaybeObjectId = do
  t <- getInt32Int
  if t < 1
    then pure Nothing
    else pure . Just . ObjectId $ t

normaliseObjectTypeMaybe :: Int -> Maybe ObjectType
normaliseObjectTypeMaybe 65535 = Nothing
normaliseObjectTypeMaybe i = Just $ normaliseObjectType i

showRemainingBytes :: Get ()
showRemainingBytes = do
  t <- G.lookAhead (G.getRemainingLazyByteString)
  traceShowBytesM $ BL.toStrict t

showNextNBytes :: Int -> Get ()
showNextNBytes n = do
  t <- G.lookAhead (G.getByteString n)
  traceShowBytesM t

traceShowBytesM :: (Monad m) => ByteString -> m ()
traceShowBytesM bs = do
  let bs' = BL.toStrict .  Builder.toLazyByteString . Builder.byteStringHex $ bs
      t = T.intercalate " " $ T.chunksOf 2 (decodeLatin1 bs')
  traceShowM t