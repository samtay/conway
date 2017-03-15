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
  -- * Construction
  , board
  , expand
  , contract
  -- * Running the game
  , step
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
  , lazyGridMapIndexed
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

-- | Create a board with given height, length, and initial state
board :: Int -- ^ Height
      -> Int -- ^ Length
      -> [Cell] -- ^ List of cells initially alive
      -> Board
board h l =
  foldr
    (\c g -> GM.insert (normalise g c) Alive g)
    (lazyGridMap (torOctGrid h l) (repeat Dead))

-- | Expand a board by specified number of rows/columns in north/east/south/west directions
expand :: Int -- ^ Top
       -> Int -- ^ Right
       -> Int -- ^ Bottom
       -> Int -- ^ Left
       -> Board -- ^ Board to expand
       -> Board
expand n e s w b = lazyGridMapIndexed (torOctGrid nh nl) vs
  where (h,l)                = size b
        (nh, nl)             = (h + n + s, l + e + w)
        vs                   = map translate . add (n + s) (e + w) $ GM.toList b
        add yn xn            = (++) [((x+l,y+h), Dead) | x <- [0..xn - 1], y <- [0..yn - 1]]
        translate ((x,y), v) = let nx = (x + w) `mod` nl
                                   ny = (y + s) `mod` nh
                                in ((nx, ny), v)

contract :: Int -- ^ Top
         -> Int -- ^ Right
         -> Int -- ^ Bottom
         -> Int -- ^ Left
         -> Board -- ^ Board to contract
         -> Board
contract n e s w b = lazyGridMapIndexed (torOctGrid nh nl) vs
  where (h,l) = size b
        (nh, nl) = (h - n - s, l - e - w)
        vs = map translate . filter fits $ GM.toList b
        fits ((x,y), _) = and [x >= w, x < l - e, y >= s, y < h - n]
        translate ((x,y), v) = let nx = (x - w) `mod` nl
                                   ny = (y - s) `mod` nh
                                in ((nx, ny), v)

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
-- TODO this is obviously bad issuing a lookup for each cell
-- Use toList and then order it into list of type :: [St]
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
