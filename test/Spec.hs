module Main where

import Life hiding (board)
import qualified Life as L
import Test.Hspec

-- | Sample size
sSize :: Int
sSize = 20

-- | Board size (each side)
bSize :: Int
bSize = 20

main :: IO ()
main = hspec $ do

  describe "Spaceships" $
    it "gliders result in constant population" $
      let ps = map population $ game glider
       in take sSize ps `shouldBe` replicate sSize 5

  describe "Still lifes" $ do
    it "blocks are still" $
      testStill block
    it "beehives are still" $
      testStill beehive
    it "tubs are still" $
      testStill tub

  describe "Oscillators" $ do
    it "blinkers oscillate with period 2" $
      testOscillate 2 blinker
    it "toads oscillate with period 2" $
      testOscillate 2 toad
    it "beacons oscillate with period 2" $
      testOscillate 2 beacon
    it "pentadecathlons oscillate with period 15" $
      testOscillate 15 pentadecathlon

  where
    testStill :: Board -> Expectation
    testStill b =
      take sSize (game b)
        `shouldBe` replicate sSize b

    testOscillate :: Int -> Board -> Expectation
    testOscillate p b =
      take (sSize * p) (game b)
        `shouldBe` (concat $ replicate sSize $ take p $ game b)

-- spaceships

glider :: Board
glider = board [(0,0), (0,1), (0,2), (1,2), (2,1)]

-- still lifes

block :: Board
block = board [(0,0), (0,1), (1,0), (1,1)]

beehive :: Board
beehive = board [(1,2), (2,1), (2,3), (3,1), (3,3), (4,2)]

tub :: Board
tub = board [(1,2), (2,1), (2,3), (3,2)]

-- oscillators

blinker :: Board
blinker = board [(1,2), (2,2), (3,2)]

toad :: Board
toad = board [(1,2), (2,2), (3,2), (2,3), (3,3), (4,3)]

beacon :: Board
beacon = board [(1,4), (1,3), (2,4), (3,1), (4,1), (4,2)]

pentadecathlon :: Board
pentadecathlon = board [ (1,2), (2,2), (3,1), (3,3), (4,2), (5,2)
                       , (6,2), (7,2), (8,1), (8,3), (9,2), (10,2)
                       ]

-- testing utilities

board :: [Cell] -> Board
board = L.board bSize bSize

game :: Board -- ^ Initial board
     -> [Board] -- ^ Resulting game
game = iterate step
