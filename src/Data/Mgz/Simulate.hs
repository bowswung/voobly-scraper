module Data.Mgz.Simulate where

import RIO

import Data.Mgz.Deserialise


data GameState

simulate :: RecInfo -> GameState
simulate _rinfo = error "asdf"



replay :: HasLogFunc env => GameState -> RIO env ()
replay _ =
  logInfo "Game start"
