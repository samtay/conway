{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import Life
import qualified Life.Examples as LE

import Brick
import Brick.BChan
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , hBox
  , withBorderStyle
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.ProgressBar as P
import Brick.Widgets.Border.Style (unicodeBold)

-- Game State

-- | Game state
data Game = Game { _board   :: Board -- ^ Board state
                 , _time    :: Int   -- ^ Time elapsed
                 , _paused  :: Bool  -- ^ Playing vs. paused
                 , _speed   :: Float -- ^ Speed in [0..1]
                 , _counter :: Int   -- ^ Counter of 'Tick' events (used to control speed)
                 } -- deriving (Eq, Show)

-- | Initial game with empty board
initialGame :: Game
initialGame = Game { _board   = board minG minG []
                   , _time    = 0
                   , _paused  = True
                   , _speed   = 0.5
                   , _counter = 0
                   }

-- | Speed increments = 0.01 gives 100 discrete speed settings
speedInc :: Float
speedInc = 0.01

-- | Minimum interval speed (microseconds)
--
-- Corresponding speed == 4 frames / second
minInterval :: Int
minInterval = 100000

-- | Maximum interval speed (microseconds)
--
-- Corresponding speed == 0.5 frames / second
maxInterval :: Int
maxInterval = 2000000

-- | Mid interval speed (microseconds)
midInterval :: Int
midInterval = (maxInterval - minInterval) `div` 2 + minInterval

-- Interface

-- | Tick is exactly what it sounds like - the tick of the counter event stream
-- It in and of itself does not "count" anything and thus is not a counter
data Tick = Tick

-- TODO - possibly add Name'd resources

app :: App Game Tick ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor -- TODO keep track of "focus" in state
                                              -- and implement cursor chooser based on that
                                              -- although.. prob dont need cursor?
          , appHandleEvent = handleEvent
          , appStartEvent = return -- TODO setup grid size here!
          , appAttrMap = const $ gameAttrMap
          }

---- Drawing

drawUI :: Game -> [Widget ()]
drawUI g = [drawGrid g <=> drawSpeedBar g]

drawGrid :: Game -> Widget ()
drawGrid _ =
  withBorderStyle unicodeBold $
  B.borderWithLabel (txt "Game of Life") $
  C.center $
  txt "testing testing testing!" -- TODO make grid

drawSpeedBar :: Game -> Widget ()
drawSpeedBar g = P.progressBar (Just $ "Speed: " <> show (fromEnum $ _speed g * 100)) (_speed g)

-- TODO look in visibility demo for rendering grid
renderSt :: St -> Widget ()
renderSt s = undefined -- TODO make a 1x1 box colored or empty

gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
              [ (P.progressIncompleteAttr, V.blue `on` V.yellow)
              , (P.progressCompleteAttr,   V.blue `on` V.green)
              ]

-- | Cell width
cw :: Int
cw = 2

-- | Min grid side
minG :: Int
minG = 20

---- Events

-- TODO look in mouse demo for handling mouse events in different layers!
handleEvent :: Game -> BrickEvent () Tick -> EventM () (Next Game)
handleEvent g (AppEvent Tick) = undefined
handleEvent g (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = continue $ g { _speed = validS $ (_speed g) + speedInc }
handleEvent g (VtyEvent (V.EvKey V.KLeft [V.MCtrl])) = continue $ g { _speed = validS $ (_speed g) - speedInc }
-- wow need TH NOW!
handleEvent g _ = halt g

validS :: Float -> Float
validS = clamp 0 1

-- | Get interval from progress bar float (between 1 and
speedToInterval :: RealFrac a => a -> Int
speedToInterval x
  | 0 <= x && x >= 1 =
    floor $ (fromIntegral $ maxInterval - minInterval) * (1 - x) + fromIntegral minInterval
  | x < 0            = maxInterval
  | x > 0            = minInterval

-- Runtime

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay midInterval
  defaultMain app initialGame >>= printResult

printResult :: Game -> IO ()
printResult g = mapM_ putStrLn
  [ "Your game ended with"
  , "  population: " <> p
  , "        time: " <> t
  ]
    where p = show $ population . _board $ g
          t = show $ _time g
          -- TODO TemplateHaskell


-- Ctrl+Return to pause/play simulation
-- Return to move forward 1 step in simulation
-- Little floating box with current time & population
-- Grid with squares that can be traversed via arrow keys
-- Spacebar to toggle Alive / Dead
-- Mouse click on
  -- cell -> toggle Alive/Dead
  -- button that says "Play/Pause (Enter)" -> play/pause simulation
  -- take a look at MouseDemo.hs -- probably need layer for each box?
-- Clear button (c)
-- Viewbox with numbered list of examples
  -- scrollable with Ctrl + up/down
  -- Ctrl+N will load up the Nth example (and pause it)
-- ProgressBar for speed setting from 0 - 100
  -- Ctrl + left/right to change speed setting
-- Change grid size on terminal resize (& start grid size based on this)
  -- Ah. We need custom widgets for contextual info: https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#implementing-custom-widgets
-- Small text at the bottom with current grid size, e.g. 200 x 220

-- Possible features
  -- Typeclass for cellular automata ca g st (only 2d ?, maybe 2d subclass ?)
    -- methods: size :: (Int, Int)
    --          step :: (ca -> ca)
    --          rule :: (g -> st -> st) ? hmm need to think more about this
  -- Allow two list boxes that specify exactly how many live neighbors results in Live cell,
     -- for alive/dead cells respectively
     -- cool for people exploring rules of cellular automata

-- Questions/Thoughts
  -- Do I need or want itemfield package?
  -- Perhaps avoid Ctrl + arrows due to https://github.com/jtdaugherty/brick/blob/master/FAQ.md
  -- Profile via criterion before asking r/haskell for performance advice
