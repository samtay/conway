{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Lens.Micro
       (ix, over, set, to, (%~), (&), (.~), (<&>), (^.), (^?), _1, _2)
import Lens.Micro.TH

import           Life hiding (board)
import qualified Life as L
import qualified Life.Examples as LE

import           Brick
import           Brick.BChan
import qualified Brick.Focus as F
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
                 ( emptyWidget
                 , hBox
                 , padLeftRight
                 , padTopBottom
                 , withBorderStyle
                 , (<+>)
                 , (<=>)
                 )
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V

-- | Name resources (needed for scrollable viewport)
data Name = GridVP | ExampleVP
  deriving (Ord, Show, Eq)

-- | Game state
data Game = Game
  { _board    :: Board -- ^ Board state
  , _time     :: Int -- ^ Time elapsed
  , _paused   :: Bool -- ^ Playing vs. paused
  , _speed    :: Float -- ^ Speed in [0..1]
  , _interval :: TVar Int -- ^ Interval kept in TVar
  , _focus    :: F.FocusRing Name -- ^ Keeps track of grid focus
  , _selected :: Cell -- ^ Keeps track of cell focus
  }

makeLenses ''Game

-- | Initial game with empty board
initialGame :: TVar Int -> Game
initialGame tv = Game { _board    = L.board 20 20 []
                      , _time     = 0
                      , _paused   = True
                      , _speed    = initialSpeed
                      , _interval = tv
                      , _focus    = F.focusRing [GridVP, ExampleVP]
                      , _selected = (0,19)
                      }

initialSpeed :: Float
initialSpeed = 0.75

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
-- Corresponding speed == 1 frames / second
maxI :: Int
maxI = 1000000

-- | Mid interval (microseconds)
midI :: Int
midI = (maxI - minI) `div` 2 + minI

-- Interface

-- | Tick is exactly what it sounds like - the tick of the counter event stream
-- It in and of itself does not "count" anything and thus is not a counter
data Tick = Tick

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor -- TODO keep track of "focus" in state
                                              -- and implement cursor chooser based on that
                                              -- although.. prob dont need cursor?
          , appHandleEvent = handleEvent
          , appStartEvent = return -- TODO setup grid size here!
          , appAttrMap = const $ gameAttrMap
          }

---- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = [ vBox [ drawGrid g
                  , hBox $ vLimit 10 . padTopBottom 1
                    <$> [ drawSpeedBar (g^.speed) <=> drawInstruct
                        , drawPButton (g^.paused) <=> drawCButton
                        , drawExamples
                        ]
                   ]
           ]

-- | Draw grid
--
-- BIG asterisk *** I wanted this to be reasonably performant,
-- so I'm leveraging the fact that 'toList' returns ordered tiles.
drawGrid :: Game -> Widget n
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Game of Life")
  $ C.center
  $ fst $ toCols (emptyWidget, g ^. board ^. to toMap)
    where toCols :: (Widget n, [(Cell, St)]) -> (Widget n, [(Cell, St)])
          toCols (w,[]) = (w,[])
          toCols (w,xs) = let (c,cs) = splitAt rowT xs
                           in toCols (w <+> mkCol c, cs)

          mkCol :: [(Cell, St)] -> Widget n
          mkCol = foldr (flip (<=>) . renderSt) emptyWidget

          rowT :: Int
          rowT  = g ^. board ^. to size ^. _2

          selCell :: Maybe Cell
          selCell = if (g^.focus^. to F.focusGetCurrent == Just GridVP)
                       then Just (normalize (g^.board) $ g^.selected)
                       else Nothing

          renderSt :: (Cell, St) -> Widget n
          renderSt (c, Alive) = addSelAttr c $ withAttr aliveAttr cw
          renderSt (c, Dead)  = addSelAttr c $ withAttr deadAttr cw

          addSelAttr :: Cell -> Widget n -> Widget n
          addSelAttr c = if selCell == Just c then forceAttr selectedAttr else id


drawSpeedBar :: Float -> Widget n
drawSpeedBar s =
  padBottom (Pad 1) $
  P.progressBar (Just lbl) s
    where lbl = "Speed: "
              <> show (fromEnum $ s * 100)
              <> "  "
              <> "(Ctrl <-,->)"

drawInstruct :: Widget n
drawInstruct = padBottom Max $ str $
  "Press 'space' to toggle play/pause,  'n' to take 1 step,\n\
  \Ctrl(left, right) to vary speed,     'c' to clear the board,\n\
  \Ctrl(up, down) to scroll examples,   '1,2,..' to draw an example,\n\
  \(left,right,up,down) to scroll grid, 'Enter' to toggle cell state,\n\
  \'+_=-' to expand/contract horizontally/vertically,\n\
  \'Tab' to move focus,                  and ESC to quit."

drawPButton :: Bool -> Widget n
drawPButton pause = mkButton $
  if pause
     then withAttr pausedAttr $ str "Play (Space)"
     else withAttr playingAttr $ str "Pause (Space)"

drawCButton :: Widget n
drawCButton = mkButton $ str "Clear (c)"

drawExamples :: Widget Name
drawExamples =
  withAttr examplesAttr $
  mkBox BS.unicodeRounded "Examples (Press #)" $
  vLimit 4 $ hLimit 19 $
  viewport ExampleVP Vertical $
  padRight Max $
  str $ unlines $ zipWith lbl [0..] examples
    where lbl n (s, _) = show n ++ ". " ++ s

examples :: [(String, (Int -> Int -> Board))]
examples =
  [ ("Glider", LE.glider)
  , ("Pentadecathlon", LE.pentadecathlon)
  , ("Beacon", LE.beacon)
  , ("Toad", LE.toad)
  , ("Blinker", LE.blinker)
  , ("Tub", LE.tub)
  , ("Beehive", LE.beehive)
  , ("Block", LE.block)
  ]

mkButton :: Widget n -> Widget n
mkButton = B.border . withBorderStyle BS.unicodeRounded . padLeftRight 1

mkBox :: BS.BorderStyle -> String -> Widget n -> Widget n
mkBox bs s = withBorderStyle bs . B.borderWithLabel (str s)

aliveAttr, deadAttr, selectedAttr :: AttrName
aliveAttr = "alive"
deadAttr = "dead"
selectedAttr = "selected"

pausedAttr, playingAttr :: AttrName
pausedAttr = "paused"
playingAttr = "playing"

examplesAttr :: AttrName
examplesAttr = "examples"

gameAttrMap :: AttrMap
gameAttrMap = attrMap V.defAttr
              [ (aliveAttr,                bg V.white)
              , (deadAttr,                 bg V.black)
              , (selectedAttr,             bg V.cyan)
              , (pausedAttr,               fg V.green)
              , (playingAttr,              fg V.red)
              , (examplesAttr,             fg V.blue)
              , (P.progressIncompleteAttr, V.blue `on` V.yellow)
              , (P.progressCompleteAttr,   V.blue `on` V.green)
              ]

-- | Cell widget
cw :: Widget n
cw = str "  "

---- Events

-- TODO look in mouse demo for handling mouse events in different layers!
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $
  if (g^.paused || g^.speed == 0)
     then g
     else forward g
handleEvent g (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = handleSpeed g (+)
handleEvent g (VtyEvent (V.EvKey V.KLeft [V.MCtrl]))  = handleSpeed g (-)
handleEvent g (VtyEvent (V.EvKey V.KUp [V.MCtrl]))    = scrollEx (-1) >> continue g
handleEvent g (VtyEvent (V.EvKey V.KDown [V.MCtrl]))  = scrollEx 1 >> continue g
handleEvent g (VtyEvent (V.EvKey V.KRight []))        = handleMove g (over _1 succ)
handleEvent g (VtyEvent (V.EvKey V.KLeft []))         = handleMove g (over _1 pred)
handleEvent g (VtyEvent (V.EvKey V.KUp []))           = handleMove g (over _2 succ)
handleEvent g (VtyEvent (V.EvKey V.KDown []))         = handleMove g (over _2 pred)
handleEvent g (VtyEvent (V.EvKey V.KEnter []))        = onlyWhenFocused g GridVP $ handleSel g
handleEvent g (VtyEvent (V.EvKey (V.KChar '\t') []))  = continue $ g & focus %~ F.focusNext
handleEvent g (VtyEvent (V.EvKey (V.KChar 'n') []))   = continue $ forward g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))   = continue $ g & paused %~ not
handleEvent g (VtyEvent (V.EvKey (V.KChar 'c') []))   = continue $ g & board %~ fmap (const Dead)
handleEvent g (VtyEvent (V.EvKey (V.KChar '-') []))   = continue $ g & board %~ resize 0 (-1)
handleEvent g (VtyEvent (V.EvKey (V.KChar '_') []))   = continue $ g & board %~ resize (-1) 0
handleEvent g (VtyEvent (V.EvKey (V.KChar '=') []))   = continue $ g & board %~ resize 0 1
handleEvent g (VtyEvent (V.EvKey (V.KChar '+') []))   = continue $ g & board %~ resize 1 0
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') []))   = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar n) []))
  | n `elem` ['0'..'9']                               = handleExample g n
  | otherwise                                         = continue g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))          = halt g
handleEvent g _                                       = continue g

forward :: Game -> Game
forward = (& board %~ step) . (& time %~ succ)

handleSpeed :: Game -> (Float -> Float -> Float) -> EventM n (Next Game)
handleSpeed g (+/-) = do
  let newSp = validS $ (g^.speed) +/- speedInc
  liftIO $ atomically $ writeTVar (g^.interval) (spToInt newSp)
  continue $ g & speed .~ newSp

handleMove :: Game -> (Cell -> Cell) -> EventM Name (Next Game)
handleMove g mv = onlyWhenFocused g GridVP $ continue $
     g & selected %~ (normalize (g^.board) . mv)

handleSel :: Game -> EventM Name (Next Game)
handleSel g = handleMove
  (g & board %~ (adjust toggle $ g^.selected))
  (over _1 succ)

handleExample :: Game -> Char -> EventM n (Next Game)
handleExample g n = continue $ fromMaybe g mg
  where mg = set time 0 .  set paused True
              <$> (set board <$> (me <*> Just l <*> Just h) <*> Just g)
        me    = examples ^? ix (read [n]) <&> snd
        (l,h) = g ^. board . to size


scrollEx :: Int -> EventM Name ()
scrollEx n = (viewportScroll ExampleVP) `vScrollBy` n

validS :: Float -> Float
validS = clamp 0 1

toggle :: St -> St
toggle Alive = Dead
toggle Dead  = Alive

-- | Get interval from progress bar float
spToInt :: Float -> Int
spToInt = floor . toInterval . validS
  where toInterval x = (fromIntegral $ maxI - minI) * (1 - x)
                        + fromIntegral minI

onlyWhenFocused :: Game -> Name -> EventM Name (Next Game) -> EventM Name (Next Game)
onlyWhenFocused g n act = if (g ^. focus ^. to F.focusGetCurrent == Just n)
                             then act
                             else continue g

-- Runtime

main :: IO ()
main = do
  chan <- newBChan 10
  tv   <- atomically $ newTVar (spToInt initialSpeed)
  forkIO $ forever $ do
    writeBChan chan Tick
    int <- atomically $ readTVar tv
    threadDelay int
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app (initialGame tv) >>= printResult

printResult :: Game -> IO ()
printResult g = mapM_ putStrLn
  [ "Your game ended with"
  , "  population: " <> p
  , "        time: " <> t
  ]
    where p = show $ population $ g^.board
          t = show $ g^.time

-- Little floating box with current time & population
-- Small text at the bottom with current grid size, e.g. 200 x 220
-- Change grid size on terminal resize (& start grid size based on this)
  -- Ah. We need custom widgets for contextual info: https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#implementing-custom-widgets
