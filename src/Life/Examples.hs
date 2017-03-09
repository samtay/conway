-----------------------------------------------------------------------------
-- |
-- Module      :  Life.Examples
-- Copyright   :  (c) Sam Tay 2017
-- License     :  BSD3
-- Maintainer  :  sam.chong.tay@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports examples of initial 'Cell' configurations.
-- Note these are /only/ cells and /not/ boards, as I don't want to box the
-- user into a particular sized grid. Use 'Life.board' and pass an example
-- set of starting cells to get a board. For example:
--
-- >ghci> blinker
-- >[(1,2),(2,2),(3,2)]
-- >ghci> board 10 10 blinker
-- >lazyGridMap (torOctGrid 10 10) [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
--
-- However, make sure the size of the board can handle the example
-- that you want to run. The pentadecathlon is a cool oscillator,
-- but to run it successfully through all 15 steps in its period,
-- it requires a bounding box of at least 16x9. Since boards are implemented
-- using a toroidal grid (with modular, or periodic, boundaries),
-- the following suffices without worrying about initial cells in the \"center\":
--
-- >ghci> board 16 9 pentadecathlon
-- >lazyGridMap (torOctGrid 16 9) [Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Alive,Alive]
--
-----------------------------------------------------------------------------
module Life.Examples
  (
  -- * Still lifes
    block
  , beehive
  , tub
  -- * Oscillators
  , blinker
  , toad
  , beacon
  , pentadecathlon
  -- * Spaceships
  , glider
  ) where

import Life (Cell)

block :: [Cell]
block = [(0,0), (0,1), (1,0), (1,1)]

beehive :: [Cell]
beehive = [(1,2), (2,1), (2,3), (3,1), (3,3), (4,2)]

tub :: [Cell]
tub = [(1,2), (2,1), (2,3), (3,2)]

blinker :: [Cell]
blinker = [(1,2), (2,2), (3,2)]

toad :: [Cell]
toad = [(1,2), (2,2), (3,2), (2,3), (3,3), (4,3)]

beacon :: [Cell]
beacon = [(1,4), (1,3), (2,4), (3,1), (4,1), (4,2)]

pentadecathlon :: [Cell]
pentadecathlon = [ (1,2), (2,2), (3,1), (3,3), (4,2), (5,2)
                 , (6,2), (7,2), (8,1), (8,3), (9,2), (10,2)
                 ]

glider :: [Cell]
glider = [(0,0), (0,1), (0,2), (1,2), (2,1)]

