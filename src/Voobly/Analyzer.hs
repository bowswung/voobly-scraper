module Voobly.Analyzer where

import RIO
import qualified RIO.Text as T

import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString as AP
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
  _ <- AP.take 4
  headerRaw <- AP.take (headerLen - 8)
  let headerInflated = Zlib.decompress (BL.fromStrict headerRaw)
  case AP.eitherResult $ AP.parse parseInflatedHeader (BL.toStrict headerInflated) of
    Left err -> fail err
    Right h -> pure h

ignore :: a -> b -> Maybe c
ignore _ _ = Nothing

parseInflatedHeader :: AP.Parser Header
parseInflatedHeader = do
  headerVersion <- takeText 8
  bs <- parseFloat 4

  traceShowM $  headerVersion
  traceShowM $  bs
  error "ADASD"
  --pure Header{..}



takeText :: Int -> AP.Parser Text
takeText n = fmap ((T.dropAround (== '\0')) . decodeLatin1) $ AP.take n


parseFloat :: Int -> AP.Parser Float
parseFloat n = do
  t <- AP.take n
  pure $ (G.runGet G.getFloatle (BL.fromStrict t))

parseInt8 :: AP.Parser Int
parseInt8 = do
  t <- AP.take 1
  pure . fromIntegral $ (G.runGet G.getInt8 (BL.fromStrict t))

parseInt32 :: AP.Parser Int
parseInt32 = do
  t <- AP.take 4
  pure . fromIntegral $ (G.runGet G.getInt32le (BL.fromStrict t))

traceShowBytesM :: (Monad m) => ByteString -> m ()
traceShowBytesM bs = do
  traceShowM $ Builder.toLazyByteString . Builder.byteStringHex $ bs
