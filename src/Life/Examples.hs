-----------------------------------------------------------------------------
-- |
-- Module      :  Life.Examples
-- Copyright   :  (c) Sam Tay 2017
-- License     :  BSD3
-- Maintainer  :  sam.chong.tay@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports examples of initial configurations.
-- The grid size is required to create these boards so that the example
-- can be \"centered\" on the grid when rendered (even though the
-- toroidal grid itself does not have a \"center\").
-- For example:
--
-- >ghci> blinker 9 9
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|X|X|X|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
-- >_|_|_|_|_|_|_|_|_
--
-- However, make sure the size of the board can handle the example
-- that you want to run. The pentadecathlon is a cool oscillator,
-- but to run it successfully through all 15 steps in its period,
-- it requires a bounding box of at least 11x17. Since boards are implemented
-- using a toroidal grid and coordinates are normalised upon creation,
-- you might not realize anything is wrong:
-- >ghci> let p = pentadecathlon 11 17
-- >ghci> p == (iterate step p !! 15)
-- >True
-- >ghci> let p' = pentadecathlon 11 16
-- >ghci> p' == (iterate step p' !! 15)
-- >False
--
-- As a rule of thumb, go big or go home. Conway's Game of Life is more
-- interesting when the spacial limits tend to infinity.
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

import Life

block :: Int -> Int -> Board
block = center [(-1,-1), (-1,0), (0,-1), (0,0)]

beehive :: Int -> Int -> Board
beehive = center [(-1,0), (0,-1), (0,1), (1,-1), (1,1), (2,0)]

tub :: Int -> Int -> Board
tub = center [(-1,0), (0,-1), (0,1), (1,0)]

blinker :: Int -> Int -> Board
blinker = center [(-1,0), (0,0), (1,0)]

toad :: Int -> Int -> Board
toad = center [(-1,0), (0,0), (1,0), (0,1), (1,1), (2,1)]

beacon :: Int -> Int -> Board
beacon = center [(-1,1), (-1,2), (0,2), (1,-1), (2,0), (2,-1)]

pentadecathlon :: Int -> Int -> Board
pentadecathlon = center [ (-4,0), (-3,0), (-2,-1), (-2,1), (-1,0), (0,0)
                        , (1,0), (2,0), (3,-1), (3,1), (4,0), (5,0)
                        ]

glider :: Int -> Int -> Board
glider = center [(-1,-1), (0,-1), (0,1), (1,-1), (1,0)]

center :: [Cell] -> Int -> Int -> Board
center cs h l = board h l $
  map (\(x,y) -> (x + xoff, y + yoff)) cs
    where xoff = l `div` 2
          yoff = h `div` 2
