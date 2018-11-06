module Voobly.Analyzer where

import RIO
import qualified RIO.Text as T
import qualified RIO.List as L

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
--

parseRec :: HasLogFunc env => RIO env ()
parseRec = do
  bs <- BS.readFile "/code/recanalyst/test/recs/versions/up1.4.mgz"
  case AP.eitherResult $ AP.parse gameParser bs of
    Left err -> logError $ displayShow err
    Right h -> do
      traceShowM h
      pure ()

data Header = Header {
    headerVersion :: Int
  } deriving (Show)

gameParser :: AP.Parser Header
gameParser = do
  header <- parseHeader
  pure header


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
  showNextNBytes 100
  mapSizeX <- traceParse "mapSizeX" parseInt32
  mapSizeY <- traceParse "mapSizeY" parseInt32

  error "ADASD"
  --pure Header{..}

displayShowT ::  Show a => a -> Text
displayShowT = utf8BuilderToText . displayShow

traceParse :: (Show a) => Text -> AP.Parser a -> AP.Parser a
traceParse msg p = do
  a <- p
  traceM $ msg <> ": " <> displayShowT a
  pure a


takeText :: Int -> AP.Parser Text
takeText n = fmap ((T.dropAround (== '\0')) . decodeLatin1) $ AP.take n

skipN :: Int -> AP.Parser ()
skipN = void . AP.take

parseFloat :: Int -> AP.Parser Float
parseFloat n = do
  t <- AP.take n
  pure $ (G.runGet G.getFloatle (BL.fromStrict t))

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
