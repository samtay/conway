-----------------------------------------------------------------------------
-- |
-- Module      :  Life
-- Copyright   :  (c) Sam Tay 2017
-- License     :  BSD3
-- Maintainer  :  sam.chong.tay@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Life
  (
  -- * Types
    Board
  , Cell
  , St(..)
  , Game(..)
  -- * Construction
  , board
  -- * Running the game
  , step
  , stepG
  , population
  , gameover
  ) where

import Data.List (intersperse)

import Math.Geometry.Grid
  ( Index(..)
  , neighbours
  , size
  )
import Math.Geometry.GridInternal
  ( WrappedGrid(..)
  , normalise
  )
import Math.Geometry.Grid.Octagonal
  ( TorOctGrid(..)
  , torOctGrid
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
type Board = LGridMap TorOctGrid St
type Cell = Index Board

-- | Possible cell states
data St = Alive | Dead
  deriving (Eq, Show)

data Game =
  Game { gTime  :: Int -- ^ Time step
       , gBoard :: Board -- ^ Current board state
       } deriving (Eq, Show)

board :: Int -- ^ Height
      -> Int -- ^ Length
      -> [Cell] -- ^ List of cells initially alive
      -> Board
board h l =
  foldr
    (\c g -> GM.insert (normalise g c) Alive g)
    (lazyGridMap (torOctGrid h l) (repeat Dead))

stepG :: Game -> Game
stepG (Game t b) = Game (t + 1) (step b)

step :: Board -> Board
step b = GM.mapWithKey rule b
  where rule :: Cell -> St -> St
        rule c Dead
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead
        rule c Alive
          | liveNeighbors c == 2 = Alive
          | liveNeighbors c == 3 = Alive
          | otherwise            = Dead

        liveNeighbors :: Cell -> Int
        liveNeighbors c = population $ GM.filterWithKey (const . (`elem` neighbours b c)) $ b

-- | Returns the total number of living cells in a board
population :: Board -> Int
population = sum . map fn . GM.elems
  where fn Alive = 1
        fn Dead  = 0

-- | Check if every cell is dead (i.e., gameover)
gameover :: Board -> Bool
gameover = (== 0) . population

-- | Overlap show instance so we can see a nice grid of values
--
-- Nice when sanity checking or playing in the REPL
instance {-# OVERLAPPING #-} Show Board where
  show b = unlines . reverse $ map mkRow [0..rowT - 1]
    where (rowT, colT)      = size b

          mkRow :: Int -> String
          mkRow y = intersperse '|' $
            map (toX_ . query . (,y)) [0..colT - 1]

          query :: Cell -> Maybe St
          query = (`GM.lookup` b)

          toX_ :: Maybe St -> Char
          toX_ (Just Alive) = 'X'
          toX_ _            = '_'

-- | We should have an inherited WrappedGrid instance for LGridMap
--
-- TODO: PR submitted to author, remove this once package is updated
instance WrappedGrid g => WrappedGrid (LGridMap g v) where
  normalise gm   = normalise (GM.toGrid gm)
  denormalise gm = denormalise (GM.toGrid gm)
