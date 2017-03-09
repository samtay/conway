module Main where

import Life
import Test.Hspec

-- | Sample size
sSize :: Int
sSize = 30

-- | Glider size
gSize :: Int
gSize = 5

main :: IO ()
main = hspec $
  describe "Life rules" $
    it "gliders result in constant population" $
      let ps = map population iterateGlider
       in take sSize ps `shouldBe` replicate sSize gSize

boardWithGlider :: Board
boardWithGlider = board 10 10 [(0,0), (0,1), (0,2), (1,2), (2,1)]

iterateGlider :: [Board]
iterateGlider = iterate step boardWithGlider
