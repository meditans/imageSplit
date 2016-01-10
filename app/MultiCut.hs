{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module MultiCut where

import           Control.Lens
import           Data.List    (delete)
import           Data.List    (foldl1')
import           Data.Ord     (comparing)

-- A cut is the contiguous area between two coordinates in the native page
-- coordinate system (zero is top-left).
type Cut         = (Int,Int)

-- A multicut is a list of cuts that will be linked together in a single image.
data MultiCut = MultiCut { _cuts     :: [Cut]
                         , _selected :: Bool
                         } deriving (Eq,Show)
makeLenses ''MultiCut

instance Ord MultiCut where
  compare mc1 mc2 = comparing (preview (cuts . _head)) mc1 mc2

-- This function returns the non-selected unions of the two multicuts.
mergeMultiCuts :: MultiCut -> MultiCut -> MultiCut
mergeMultiCuts (MultiCut ms1 _) (MultiCut ms2 _) = MultiCut (ms1 ++ ms2) False

-- Add a cut to a multicut, mantaining the selectedness.
addCut :: Cut -> MultiCut -> MultiCut
addCut c = cuts %~ (c:)

-- Remove a cut from a multicut, if it's there, mantaining the selectedness.
deleteCut :: Cut -> MultiCut -> MultiCut
deleteCut c = cuts %~ (delete c)

containsCut :: Cut -> MultiCut -> Bool
containsCut c mc = c `elem` (mc^.cuts)

selectContaining :: Cut -> [MultiCut] -> [MultiCut]
selectContaining c = traversed . filtered (containsCut c) . selected .~ True

mergeSelected :: [MultiCut] -> [MultiCut]
mergeSelected mcs
  | null $ filter _selected mcs = mcs
  | otherwise = foldl1' mergeMultiCuts (filter _selected mcs) : filter (not . _selected) mcs

