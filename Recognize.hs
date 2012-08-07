module Recognize where

import Data.List (unfoldr)
import Test.HUnit
import Test.QuickCheck

import Area

type RecogText  = [[RecogChar]]
data RecogChar  = RecogChar
                | RecogSkip Int
                deriving (Show)

data MatchLetter = MatchLetter  {matchArea :: PixelArea
                                ,matchChar :: RecogChar
                                } deriving (Show)


----


findLines :: PixelArea -> [PixelArea]
findLines = undefined


recognizeText :: PixelArea -> Maybe RecogText
recognizeText = return . map recognizeLine . findLines


recognizeLine :: PixelArea -> [RecogChar]
recognizeLine = unfoldr func
  where func :: PixelArea -> Maybe (RecogChar, PixelArea)
        func px | isEmpty px = Nothing
                | otherwise  = 
                    case recognizeChar px of
                      Just (rc, len)  -> return (rc, skipColumns px len)
                      Nothing         -> return (RecogSkip 1, skipColumns px 1)


recognizeChar :: PixelArea -> Maybe (RecogChar, Int)
recognizeChar area = undefined
  where matches = filter (letterEqual area) letters


letters :: [MatchLetter]
letters = undefined


letterEqual :: PixelArea -> MatchLetter -> Bool
letterEqual area ltr = 
  case areaEqual area (matchArea ltr) of
    Just (Coord width h)  -> width == areaWidth (matchArea ltr)
    Nothing               -> False


-- TESTS

testsRecog = TestList $ map TestCase
  [assertEqual ""  1 
                   1 
  ]

prop_empty c1 = (c1::Int) == c1

runRecog = do
  runTestTT testsRecog
  quickCheck prop_coord
