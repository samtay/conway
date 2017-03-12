{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH

import Life hiding (board)
import qualified Life as L
import qualified Life.Examples as LE
import Math.Geometry.Grid (size)
import Math.Geometry.GridMap (toList)

import Brick
import Brick.BChan
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , hBox
  , withBorderStyle
  , emptyWidget
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.ProgressBar as P
import Brick.Widgets.Border.Style (unicodeBold)
import qualified Graphics.Vty as V

-- Game State

-- | Game state
data Game = Game { _board    :: Board -- ^ Board state
                 , _time     :: Int   -- ^ Time elapsed
                 , _paused   :: Bool  -- ^ Playing vs. paused
                 , _speed    :: Float -- ^ Speed in [0..1]
                 , _interval :: TVar Int
                 } -- deriving (Eq, Show)

makeLenses ''Game

-- | Initial game with empty board
initialGame :: TVar Int -> Game
initialGame tv = Game { _board    = LE.pentadecathlon 40 40 --L.board minG minG []
                      , _time     = 0
                      , _paused   = True
                      , _speed    = 0.5
                      , _interval = tv
                      }

-- | Speed increments = 0.01 gives 100 discrete speed settings
speedInc :: Float
speedInc = 0.01

-- | Minimum interval (microseconds)
--
-- Corresponding speed == 4 frames / second
minI :: Int
minI = 100000

-- | Maximum interval (microseconds)
--
-- Corresponding speed == 0.5 frames / second
maxI :: Int
maxI = 2000000

-- | Mid interval (microseconds)
midI :: Int
midI = (maxI - minI) `div` 2 + minI

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

-- | Draw grid
--
-- BIG asterisk *** I wanted this to be reasonably performant,
-- so I'm leveraging the fact that 'toList' returns ordered tiles.
--
-- Also, TODO check on itemfields package, it might make this easier
drawGrid :: Game -> Widget ()
drawGrid g =
  withBorderStyle unicodeBold $
  B.borderWithLabel (txt "Game of Life") $
  C.center $
  fst $ toCols (emptyWidget, toList $ g^.board)
    where toCols :: (Widget (), [(Cell, St)]) -> (Widget (), [(Cell, St)])
          toCols (w,[]) = (w,[])
          toCols (w,xs) = let (c,cs) = splitAt rowT xs
                           in toCols (w <+> mkCol c, cs)
          mkCol :: [(Cell, St)] -> Widget ()
          mkCol = foldr (flip (<=>) . renderSt . snd) emptyWidget
          rowT :: Int
          rowT  = fst . size $ g^.board

drawSpeedBar :: Game -> Widget ()
drawSpeedBar g = P.progressBar (Just $ "Speed: " <> show (fromEnum $ g^.speed * 100)) (g^.speed)

renderSt :: St -> Widget ()
renderSt Alive = withAttr aliveAttr cw
renderSt Dead = withAttr deadAttr cw

aliveAttr, deadAttr :: AttrName
aliveAttr = "alive"
deadAttr = "dead"


gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
              [ (aliveAttr,                bg V.white)
              , (deadAttr,                 bg V.black)
              , (P.progressIncompleteAttr, V.blue `on` V.yellow)
              , (P.progressCompleteAttr,   V.blue `on` V.green)
              ]

-- | Cell widget
cw :: Widget ()
cw = txt "  "

-- | Min grid side
minG :: Int
minG = 20

---- Events

-- TODO look in mouse demo for handling mouse events in different layers!
handleEvent :: Game -> BrickEvent () Tick -> EventM () (Next Game)
handleEvent g (AppEvent Tick) = continue $ g & board %~ step
handleEvent g (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = handleSpeed g (+)
handleEvent g (VtyEvent (V.EvKey V.KLeft [V.MCtrl])) = handleSpeed g (-)
handleEvent g _ = halt g

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM () (Next Game)
handleSpeed g (+/-) = do
  let newSp = validS $ (g^.speed) +/- speedInc
  liftIO $ atomically $ writeTVar (g^.interval) (spToInt newSp)
  continue $ g & speed .~ newSp

validS :: Float -> Float
validS = clamp 0 1

-- | Get interval from progress bar float (between 1 and
spToInt :: Float -> Int
spToInt = floor . toInterval . validS
  where toInterval x = (fromIntegral $ maxI - minI) * (1 - x)
                        + fromIntegral minI

-- Runtime

main :: IO ()
main = do
  chan <- newBChan 10
  tv   <- atomically $ newTVar midI
  forkIO $ forever $ do
    writeBChan chan Tick
    int <- atomically $ readTVar tv
    threadDelay int
  customMain (V.mkVty V.defaultConfig) (Just chan) app (initialGame tv) >>= printResult

printResult :: Game -> IO ()
printResult g = mapM_ putStrLn
  [ "Your game ended with"
  , "  population: " <> p
  , "        time: " <> t
  ]
    where p = show $ population $ g^.board
          t = show $ g^.time


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
