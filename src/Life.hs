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
  , step
  , population
  ) where

import Math.Geometry.Grid (neighbours)
import Math.Geometry.Grid.Square
  ( TorSquareGrid(..)
  , torSquareGrid
  )
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.GridMap.Lazy
  ( LGridMap
  , lazyGridMap
  )

-- | A modular game of life board
--
-- With this interpretation, for a board of size @n x n@
-- the @(n + 1)@th column/row is the same as the boundary at the @1@th column/row.
type Board = LGridMap TorSquareGrid St

-- | Possible cell states
data St = Alive | Dead
  deriving (Eq, Show)

data Game =
  Game { gTime  :: Int -- ^ Time step
       , gBoard :: Board -- ^ Current board state
       } deriving (Eq, Show)

initGame :: Int -- ^ Height
         -> Int -- ^ Length
         -> [(Int, Int)] -- ^ List of cells initially alive
         -> Game
initGame h l cs = Game 0 board
  where
    board = foldr
      (`GM.insert` Alive)
      (lazyGridMap (torSquareGrid h l) (repeat Dead))
      cs

step :: Game -> Game
step (Game t b) = Game (t + 1) $ GM.mapWithKey rule b
  where rule :: (Int, Int) -> St -> St
        rule c Dead
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead
        rule c Alive
          | liveNeighbors c == 2 = Alive
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead

        liveNeighbors :: (Int, Int) -> Int
        liveNeighbors c = population $ GM.filterWithKey (const . (`elem` neighbours b c)) $ b

-- | Returns the total number of living cells in a board
population :: Board -> Int
population = sum . map fn . GM.elems
  where fn Alive = 1
        fn Dead  = 0
