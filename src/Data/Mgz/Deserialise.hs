{-# OPTIONS -fno-warn-deprecations #-}
module Data.Mgz.Deserialise (
  module Data.Mgz.Deserialise,
  module Data.Mgz.Deserialise.BasicTypes,
  module Data.Mgz.Deserialise.Commands
  ) where

import RIO
import qualified RIO.List as L
import Control.Monad (replicateM)
import qualified RIO.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import qualified Data.Binary.Parser as G
import Data.Binary.Get (Get)
import qualified Codec.Compression.Zlib.Raw as Zlib


import Data.Mgz.Deserialise.BasicTypes
import Data.Mgz.Deserialise.Commands
import Data.Mgz.Constants
--

parseRec :: HasLogFunc env => RIO env (Either String RecInfo)
parseRec = do
  logInfo "Starting parse"
  bl <- BL.readFile "/code/voobly-scraper/test/recs/up1.4.mgz"
  pure $ runGetEither getRecInfo bl


data RecInfo = RecInfo {
  recInfoHeader :: Header,
  recInfoOps ::  [Op]
  }

getRecInfo :: Get RecInfo
getRecInfo = do
  header <- getHeader
  ops <- getOps
  --void $ mapM prettyCommand $ ops
  let _cmds = catMaybes $ map opCommand ops
      _cmdTypes = L.nub $ map commandToTypeText _cmds
  -- traceM $ displayShowT (L.sort cmdTypes)
  pure $ RecInfo header ops


data Header = Header {
    headerPlayers :: [PlayerInfo]
  , headerTiles :: [Tile]
  } deriving (Show)

getHeader :: Get Header
getHeader = do
  headerLen <- getInt32Int
  G.skip 4
  headerRaw <- G.getLazyByteString (fromIntegral $ headerLen - 8)
  case runGetEither getInflatedHeader $ Zlib.decompress headerRaw of
    Left err -> fail err
    Right h -> pure h


getInflatedHeader :: Get Header
getInflatedHeader = do
  _headerVersion <- takeText 8
  _bs <- G.getFloatle
  _includeAi <- getInt32Int
  G.skip 4
  _gameSpeed <-  getInt32Int
  G.skip 37

  _pov <-  getInt16Int
  numPlayers <-  getInt8Int -- this includes gaia
  _gameMode <-  getInt16Int
  G.skip 60
  mapSizeX <-  getInt32Int
  mapSizeY <-  getInt32Int
  _zones <- getInt32Int
  _allVisible <- getBool
  _fogOfWar <-  getBool
  tiles <- sequence $ map getTile $ [(x,y) | y <- [0 .. mapSizeY -1], x <- [0.. mapSizeX-1]]
  obstructions <-  getInt32Int
  G.skip $ 4 + obstructions * 4
  void $ replicateM obstructions $ getInt32Int >>= (G.skip . ((*) 8))
  mapSizeX2 <-  getInt32Int
  mapSizeY2 <-  getInt32Int
  G.skip $ mapSizeX2 * mapSizeY2 * 4  -- skip visibility mapSizeX2
  G.skip 4
  G.skip =<< fmap (* 27) getInt32Int
  G.skip 4
  players <- sequence $ map (getPlayerInfo numPlayers) $ map PlayerId [0..numPlayers-1]
  void $ mapM debugPlayer players
  pure $ Header players tiles

getOps :: Get [Op]
getOps = G.manyTill' simpleGet G.endOfInput































