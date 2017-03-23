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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Life
  (
  -- * Types
    Board
  , Cell
  , St(..)
  , Zipper(..)
  , Z(..) , ZDirection(..)-- todo remove ?
  -- * Construction
  , board
  , resize
  -- * Running the game
  , step
  , population
  , gameover
  ) where

import qualified Data.Foldable as F
import Data.List (nub)
import Data.Maybe (mapMaybe)

import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), (><))
import qualified Data.Sequence as S
import Control.Comonad
import Lens.Micro.TH

-- | A modular game of life board
--
-- With this interpretation, for a board of size @n x n@
-- the @(n + 1)@th column/row is the same as the boundary at the @1@th column/row.
type Board = ZZ St

-- | Indexer for the 'Board'
type Cell = (Int, Int)

-- | Possible cell states
data St = Alive | Dead
  deriving (Eq, Show)

-- | Class for a modular bounded container
--
-- Examples of functions provided for a simple one dimensional list, where appropriate
class Zipper z where
  type Index z
  type Direction z

  -- | Shift in a direction
  shift :: Direction z -> z a -> z a

  -- | Retrieve current cursor value
  cursor :: z a -> a

  -- | Retrieve current index value
  index :: z a -> Index z

  -- | Retrieve neighborhood of current cursor.
  neighborhood :: z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toList :: z a -> [a]

  -- | Like 'toList' but as a mapping with indices.
  toMap :: z a -> [(Index z, a)]

  -- | Construct zipper from list with starting cursor position, e.g. @(x:xs) -> Z [] x xs@.
  fromList :: [a] -> z a

  -- | Lookup by possibly denormalised index (still safe from modularity).
  --
  -- e.g. [1,2] ! 2 == 1
  (!) :: z a -> (Index z) -> a

  -- | Normalize @Index z@ value with respect to modular boundaries
  normalize :: z a -> (Index z) -> (Index z)

  -- | Get size (maximum of @Index z@).
  size :: z a -> (Index z)

-- | One dimensional finite list with cursor context
--
-- The first element of the sequence at '_zl' can be thought of
-- as /to the left/ of the cursor, while the last element is /to the right/
-- of the cursor. The cursor value and index are '_zc' and '_zi' respectively.
-- This can be thought of as a circle.
-- Warning: must have length greater than zero!
data Z a = Z { _zl :: S.Seq a
             , _zc :: a
             , _zi :: Int
             } deriving (Show)

data ZDirection = L | R
  deriving (Eq, Show)

newtype ZZ a = ZZ { unZZ :: Z (Z a) }

makeLenses ''Z

instance Zipper Z where
  type Index Z = Int
  type Direction Z = ZDirection
  shift d z@(Z l c i)
    | S.length l == 0 = z -- shifting length zero amounts to nothing
    | d == L          = let (x :< xs) = S.viewl l
                            ni        = (i - 1) `mod` size z
                         in Z (xs |> c) x ni
    | d == R          = let (xs :> x) = S.viewr l
                            ni        = (i + 1) `mod` size z
                         in Z (c <| xs) x ni
  cursor = _zc
  index = _zi
  neighborhood (Z l _ _) = mapMaybe (lookupS l) . nub $ [0, S.length l - 1]
  toList (Z l c i) = F.toList . S.reverse $ b >< (c <| f)
    where (f,b) = S.splitAt i l
  toMap = zip [0..] . toList
  fromList []     = error "Zipper must have length greater than zero."
  fromList (x:xs) = Z (S.reverse $ S.fromList xs) x 0
  normalize z = (`mod` (size z))
  size (Z l _ _) = S.length l + 1
  z ! k = fnd z (normalize z k)
    where fnd (Z l c i) n
            | i == n    = c
            | i < n     = S.index l $ (s - (n - i) - 1)
            | i > n     = S.index l $ n - i - 1
          s = size z


instance Functor Z where
  fmap f (Z l c i) = Z (fmap f l) (f c) i

instance Comonad Z where
  extract (Z _ c _)  = c
  extend f z@(Z l _ i) = undefined

-- | Create a board with given height, length, and initial state
board :: Int -- ^ Height
      -> Int -- ^ Length
      -> [Cell] -- ^ List of cells initially alive
      -> Board
board h l = undefined

-- | Adjusts the number of columns and rows respectively.
--
-- For example @resize (-1) 1@ will remove one column and add one row.
resize = undefined

step :: Board -> Board
step b = undefined

-- | Returns the total number of living cells in a board
population :: Board -> Int
population = undefined -- length . filter (==Alive) . ...

-- | Check if every cell is dead (i.e., gameover)
gameover :: Board -> Bool
gameover = (== 0) . population

-- | Overlap show instance so we can see a nice grid of values
--
-- Nice when sanity checking or playing in the REPL
-- TODO this is obviously bad issuing a lookup for each cell
-- Use toList and then order it into list of type :: [St]
{-
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
          -}

lookupS :: S.Seq a -> Int -> Maybe a
lookupS s n
  | 0 <= n && n < S.length s = Just $ S.index s n
  | otherwise                = Nothing
