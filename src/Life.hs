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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TupleSections #-}
module Life
  (
  -- * Types
    Board
  , Cell
  , St(..)
  , Zipper(..)
  , Z
  , ZZ
  -- * Construction
  , board
  , resize
  -- * Running the game
  , step
  , population
  , gameover
  ) where

import qualified Data.Foldable as F
import Data.List (nub, intercalate)
import Data.Maybe (mapMaybe, fromMaybe)

import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), (><))
import qualified Data.Sequence as S
import Control.Comonad
import Lens.Micro
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
  deriving (Eq)

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
  -- TODO consider keeping in Seq instead of []
  neighborhood :: z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toList :: z a -> [a]

  -- | Construct zipper from mapping (provide default value so this is always safe, no bottoms)
  fromMap :: Ord (Index z) => a -> [(Index z, a)] -> z a

  -- | Lookup by possibly denormalised index (still safe from modularity).
  --
  -- e.g. [1,2] ! 2 == 1
  (!) :: z a -> (Index z) -> a

  -- | Adjust value at specified index
  adjust :: (a -> a) -> Index z -> z a -> z a

  -- | Update value at specified index
  update :: a -> Index z -> z a -> z a
  update x = adjust (const x)

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
             } deriving (Eq, Show)

data ZDirection = L | R
  deriving (Eq, Show)

newtype ZZ a = ZZ { unZZ :: Z (Z a) }
  deriving (Eq) -- TODO possibly implement equality up to shifting

data ZZDirection = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Show)

makeLenses ''Z

instance Zipper Z where
  type Index Z = Int
  type Direction Z = ZDirection
  cursor = _zc
  index = _zi
  neighborhood (Z l _ _)
    | S.length l <= 2 = F.toList l
    | otherwise       = map (S.index l) [0, S.length l - 1]
  normalize z = (`mod` (size z))
  size (Z l _ _) = S.length l + 1
  (!) z@(Z l c i) = maybe c (S.index l) . zToLix z
  adjust f k z@(Z l c i) =
    maybe
      (Z l (f c) i)
      (\j -> Z (S.adjust f j l) c i)
      (zToLix z k)
  toList (Z l c i) = F.toList . S.reverse $ b >< (c <| f)
    where (f,b) = S.splitAt i l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = Z (S.fromList ys) (iToa 0) 0
    where ys = map iToa rng
          iToa i = fromMaybe a $ lookup i m
          l    = maximum . (0:) $ map fst m
          rng  = if l == 0 then [] else [l,(l-1)..1]
  shift d z@(Z l c i)
    | S.length l == 0 = z -- shifting length zero amounts to nothing
    | d == L          = Z (xs |> c) x xi
    | d == R          = Z (c <| xs) x yi
    where
      (x :< xs) = S.viewl l
      (ys :> y) = S.viewr l
      xi        = (i - 1) `mod` size z
      yi        = (i + 1) `mod` size z

-- | Transform 'Index z' into the index of the @S.Seq@ that @z@ contains
-- unless it is equivalent to current index.
zToLix :: Z a -> Int -> Maybe Int
zToLix z@(Z _ _ i) k
  | i == n = Nothing
  | i < n  = Just $ s - (n - i) - 1
  | i > n  = Just $ n - i - 1
  where n = k `mod` s
        s = size z

instance Functor Z where
  fmap f (Z l c i) = Z (fmap f l) (f c) i

instance Comonad Z where
  extract (Z _ c _)  = c
  duplicate z@(Z _ _ i) = Z (S.fromFunction (size z - 1) fn) z i
    where fn k = compose (k + 1) (shift L) $ z

instance Functor ZZ where
  fmap f = ZZ . (fmap . fmap) f . unZZ

-- | This interpretation is a 2D zipper (Z (Z a)).
--
-- The outer layer is a zipper of columns (x coordinate),
-- and each column is a zipper of @a@ values (y coordinate).
-- Warning: Keep inner column sizes consistent!
instance Zipper ZZ where
  type Index ZZ = (Int, Int)
  type Direction ZZ = ZZDirection
  cursor = _zc . _zc . unZZ
  toList = concatMap toList . toList . unZZ
  shift = undefined -- shifting up/down should shift all columns up/down
  adjust f k (ZZ z) = undefined
  normalize z (x,y) = (nx, ny)
    where nx = x `mod` z ^. to size ^. _1
          ny = y `mod` z ^. to size ^. _2
  (!) zz@(ZZ z) (x, y) = getAtIx c mk
    where (mj, mk) = zzToLix zz (x,y)
          c        = getAtIx z mj
  size (ZZ z) = (x, y)
    where x = size z
          y = z ^. zc ^. to size
  index (ZZ z) = (x, y)
    where x = z ^. zi
          y = z ^. zc ^. zi
  neighborhood (ZZ (Z cs c x)) = cNSs ++ cEs ++ cWs
    where cNSs = neighborhood c
          cWs  = case S.viewl cs of
                   (cw :< _) -> (cw ^. zc) : neighborhood cw
                   _         -> []
          cEs  = case S.viewr cs of
                   (_ :> ce) -> (ce ^. zc) : neighborhood ce
                   _         -> []
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = ZZ $ Z (S.fromList cs) (iToc 0) 0
    where cs        = map iToc rc
          iToc i    = fromMap a . insDef . map (& _1 %~ snd) $ filter ((==i) . fst . fst) m
          l         = maximum . (0:) $ map (fst . fst) m
          h         = maximum . (0:) $ map (snd . fst) m
          rc        = if l == 0 then [] else [l,(l-1)..1]
          insDef xs = if h `elem` (map fst xs) then xs else (h,a) : xs

zzToLix :: ZZ a -> (Int, Int) -> (Maybe Int, Maybe Int)
zzToLix (ZZ z) (j,k) = (mj, mk)
  where mj = zToLix z j
        c  = getAtIx z mj
        mk = zToLix c k

-- | Helper function for internal use. Expects @Int@ argument to be
-- normalized already (probably returned from *ToLix)
getAtIx :: Z a -> Maybe Int -> a
getAtIx z = maybe (z ^. zc) (z ^. zl ^. to S.index)

-- | Create a board with given height, length, and initial state
board :: Int -- ^ Length
      -> Int -- ^ Height
      -> [Cell] -- ^ List of cells initially alive
      -> Board
board l h cs = fromMap Dead $ map (,Alive) cs

  --(\c -> (,) c $ if c `elem` cs then Dead else Alive)
 -- [(x,y) | x <- [0..l-1], y <- [0..h-1]]

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

-- Nice when sanity checking or playing in the REPL
--
-- Warning: slow since I implement outer layer as column
-- and this must be rendered first as rows.
instance Show a => Show (ZZ a) where
  show z = unlines $ map mkRow [rowT - 1,rowT - 2..0]
    where (colT, rowT) = size z
          mkRow y      = intercalate "|"
            $ map (show . (z !) . (,y)) [0..colT - 1]

instance Show St where
  show Alive = "X"
  show Dead  = "_"

lookupS :: S.Seq a -> Int -> Maybe a
lookupS s n
  | 0 <= n && n < S.length s = Just $ S.index s n
  | otherwise                = Nothing

compose :: Int -> (a -> a) -> (a -> a)
compose = (foldr (.) id .) . replicate
