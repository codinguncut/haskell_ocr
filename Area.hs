module Area where

import qualified Data.Array.Unboxed as Arr
import Data.Array.Unboxed ((!))
import Control.Monad (liftM2)
import Test.HUnit
import Test.QuickCheck

type PixelArray = Arr.UArray (Int,Int) Int
data PixelArea  = PixelArea {pPixels      :: PixelArray
                            ,pTopLeft     :: Coord
                            ,pBottomRight :: Coord 
                            -- bottomRight is the first coordinate that is
                            --   not included in the area, lies outside
                            } deriving (Show)

isEmpty :: PixelArea -> Bool
isEmpty area = (areaWidth area) == 0 || (areaHeight area) == 0


areaExtent :: PixelArea -> Coord
areaExtent area = (pBottomRight area) |-| (pTopLeft area)


areaWidth :: PixelArea -> Int
areaWidth = cx . areaExtent


areaHeight :: PixelArea -> Int
areaHeight = cy . areaExtent


areaEqual :: PixelArea -> PixelArea -> Maybe Coord
areaEqual area1 area2 = 
  if and (zipWith (==) values1 values2)
    then return minExtent
    else Nothing
  where (tl1,tl2)   = (pTopLeft area1, pTopLeft area2)
        minExtent   = minCoord (areaExtent area1) (areaExtent area2)
        offsets     = [Coord x y  | x <- [0..(cx minExtent)-1]
                                  , y <- [0..(cy minExtent)-1]] 
        indices1    = map ((\a -> (cy a,cx a)) . (tl1 |+|)) offsets
        indices2    = map ((\a -> (cy a,cx a)) . (tl2 |+|)) offsets
        values1     = map ((pPixels area1)!) indices1
        values2     = map ((pPixels area2)!) indices2


skipColumns :: PixelArea -> Int -> PixelArea
skipColumns area i = area{pTopLeft = Coord (x+offset) y}
  where (Coord x y) = pTopLeft area
        width       = areaWidth area
        offset      = min width i




data Coord = Coord  {cx :: Int
                    ,cy :: Int
                    } deriving (Show,Eq)

(|-|) :: Coord -> Coord -> Coord
(|-|) c1 c2 = Coord {cx = (cx c1) - (cx c2)
                    ,cy = (cy c1) - (cy c2)
                    }

(|+|) :: Coord -> Coord -> Coord
(|+|) c1 c2 = Coord {cx = (cx c1) + (cx c2)
                    ,cy = (cy c1) + (cy c2)
                    }

minCoord :: Coord -> Coord -> Coord
minCoord c1 c2 = Coord  {cx = min (cx c1) (cx c2)
                        ,cy = min (cy c1) (cy c2)
                        }


-- TESTS

makeArray :: Coord -> [Int] -> PixelArray
makeArray coord xs = Arr.listArray ((0,0),(cy coord, cx coord)) xs 

tests = TestList $ map TestCase
  [assertEqual "areaWidth"  2 
                            (areaWidth area)
  ,assertEqual "areaHeight" 3 
                            (areaHeight area)
  ,assertEqual "minCoord"   (Coord 2 3) 
                            (minCoord (Coord 2 5) (Coord 7 3))
  ,assertEqual "areaEqual"  (Just (Coord 2 3))
                            (areaEqual area1 area1)
  ,assertEqual "areaEqual"  Nothing
                            (areaEqual area1 area2)
  ,assertEqual "skipColumns"  (Coord 1 0)
                              (pTopLeft $ skipColumns area 1)
  ]
  where area = PixelArea  {pPixels      = emptyArea
                          ,pTopLeft     = Coord 0 0
                          ,pBottomRight = Coord 2 3
                          }
        area1 = area{pPixels = makeArray (Coord 1 2) [1,2,3,4,5,6]}
        area2 = area{pPixels = makeArray (Coord 1 2) [2,2,3,4,5,6]}
        emptyArea = Arr.listArray ((0,0),(0,0)) [] :: PixelArray

instance Arbitrary Coord where
  arbitrary = liftM2 Coord arbitrary arbitrary

prop_coord c1 c2  =  (c1 |-| c1) == (Coord 0 0)
                  && (c1 |-| (c1 |-| c2)) == c2
                  && (c1 |-| c2 |+| c2) == c1
                  && (c1 |+| (Coord 0 0)) == c1

runArea = do
  runTestTT tests
  quickCheck prop_coord
