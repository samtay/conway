module Main where

import Life hiding (board)
import qualified Life as L
import Test.Hspec

-- | Sample size
sSize :: Int
sSize = 30

-- | Board size (each side)
bSize :: Int
bSize = 10

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
      pendingWith "need to write up oscillators"
    it "toads oscillate with period 2" $
      pendingWith "need to write up oscillators"
    it "beacons oscillate with period 2" $
      pendingWith "need to write up oscillators"

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
blinker = undefined

toad :: Board
toad = undefined

beacon :: Board
beacon = undefined

-- testing utilities

board :: [Cell] -> Board
board = L.board bSize bSize

game :: Board -- ^ Initial board
     -> [Board] -- ^ Resulting game
game = iterate step

testStill :: Board -> Expectation
testStill b = take sSize (game b) `shouldBe` replicate sSize b

testOscillate :: Int -> Board -> Expectation
testOscillate n b = undefined
