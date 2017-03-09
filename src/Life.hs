{-# LANGUAGE TupleSections #-}
module Life
  (
  -- * Types
    Board
  , St(..)
  , Game
  -- * Construction
  , initGame
  -- * Running the game
  ) where

import Math.Geometry.Grid.Square (TorSquareGrid(..), torSquareGrid)
-- import Math.Geometry.GridMap
import Math.Geometry.GridMap.Lazy (LGridMap, lazyGridMapIndexed)

-- | A modular game of life board
--
-- With this interpretation, for a board of size @n x n@
-- the @(n + 1)@th column/row is the same as the boundary at the @1@th column/row.
type Board = LGridMap TorSquareGrid St

-- | Possible cell states
data St = Alive | Dead

data Game =
  Game { gTime  :: Int -- ^ Time step
       , gBoard :: Board -- ^ Current board state
       }

initGame :: Int -- ^ Height
         -> Int -- ^ Length
         -> [(Int, Int)] -- ^ List of cells initially alive
         -> Game
initGame h l cs = Game 0 board
  where
    board =
      lazyGridMapIndexed
      (torSquareGrid h l)
      (map mkAlive cs)
    mkAlive = (,Alive)
