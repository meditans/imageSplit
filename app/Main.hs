{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (writeFile)
import Lib
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Data.Maybe (fromJust)
import Debug.Trace
import Codec.Picture.RGBA8
import Control.Lens
import Turtle
import Control.Foldl (list)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Data.Maybe (catMaybes)
import Data.List (minimumBy)
import Data.List (delete)
import Data.Ord (comparing)
import Codec.Picture.Png
import Data.ByteString.Lazy (writeFile)
import Data.List (sort)

data Image = Image { _xDim      :: Int
                   , _yDim      :: Int
                   , _fileName  :: String
                   , _imageBMP  :: Picture
                   , _cutCoords :: [Int]
                   , _zoomLevel  :: Float
                   , _vTranslation :: Float
                   } deriving (Eq,Show)
makeLenses ''Image

data AppState = AppState { _images :: [Image]
                         } deriving (Eq,Show)
makeLenses ''AppState

nextImage :: AppState -> AppState
nextImage (AppState is) = AppState $ tail is ++ [head is]

prevImage :: AppState -> AppState
prevImage (AppState is) = AppState $ [last is] ++ init is

main :: IO ()
main = do
  dir  <- pwd
  imgs <- fold (grep (ends "png") (format fp <$> ls dir)) list
  app  <- fmap catMaybes . sequence $ map (fromFilePathToImage . unpack) imgs

  playIO
    (InWindow "Image Splitter" (1600,900) (0,0))
    white
    30
    (AppState app)
    drawState
    processClick
    (\_ s -> return s)

processClick :: Event -> AppState -> IO AppState
processClick (EventKey (MouseButton LeftButton)  Down noModifiers (_,y)) =
  return . (images . _head %~ maybeAddCut (round y))
processClick (EventKey (MouseButton RightButton) Down noModifiers (_,y)) =
  return . (images . _head . cutCoords %~ deleteNear (round y))
processClick (EventKey (SpecialKey  KeyRight)    Down noModifiers _) =
  return . nextImage
processClick (EventKey (SpecialKey  KeyLeft)    Down noModifiers _) =
  return . prevImage
processClick (EventKey (SpecialKey  KeyEnter)    Down noModifiers _) = \s -> do
  let (Image x y fn _ cs _ _) = s ^?! images . _head
  original <- readImageRGBA8 fn
  let newCuts = sort $ map (toImageCoord y) cs
  let cutImages = map (\(a, b) -> trimImage original (x,b-a) (0,a)) $ zip (0:newCuts) (newCuts++[y])
  mapM_ (\(im, iden) -> writeFile (fn ++ "_cut" ++ show iden ++ ".png") (encodePng im)) $ zip cutImages [1..]
  return s

processClick (EventKey (Char '+') Down noModifiers _) = return . (images . _head . zoomLevel *~  1.1)
processClick (EventKey (Char '-') Down noModifiers _) = return . (images . _head . zoomLevel //~ 1.1)
processClick (EventKey (Char '=') Down noModifiers _) = return . (images . _head . zoomLevel .~ 1)
processClick (EventKey (SpecialKey KeyUp)   Down noModifiers _) = return . (images . _head . vTranslation -~ 100)
processClick (EventKey (SpecialKey KeyDown) Down noModifiers _) = return . (images . _head . vTranslation +~ 100)

processClick _ = return

maybeAddCut :: Int -> Image -> Image
maybeAddCut i img =
  let btwn a b x = a <= x && x <= b
      semiHeight = (fromIntegral $ (img ^. yDim) `div` 2) :: Float
      zL = img ^. zoomLevel
      vT = img ^. vTranslation
      upperLimit = semiHeight    * zL + vT
      lowerLimit = (-semiHeight) * zL + vT
      inImage = btwn lowerLimit upperLimit
      i' = round $ (fromIntegral i - vT) / zL
  in if inImage (fromIntegral i) then over cutCoords (i':) img else img
  -- in undefined

deleteNear :: Int -> [Int] -> [Int]
deleteNear y xs = delete (minimumBy (comparing (\x -> abs (x-y))) xs) xs

drawState :: AppState -> IO Picture
drawState (AppState imgs) = do
  let (Image _ _ fn dt cs zm vt) = head imgs
      pic = translate 0 vt . scale zm zm $ dt
  let cs' = map (\y -> let y' = (+ vt) . (* zm) $ fromIntegral y
                       in Line [(-1000, y'), (1000, y')]) cs
  return $ Pictures $ [pic] ++ cs'

fromFilePathToImage :: String -> IO (Maybe Image)
fromFilePathToImage fp = (fmap.fmap) (\im -> let (Bitmap x y _ _) = im in Image x y fp im [] 1 0) (loadJuicy fp)

noModifiers :: Modifiers
noModifiers = Modifiers Up Up Up

toImageCoord :: Int -> Int -> Int
toImageCoord imageHeight = negate . subtract (imageHeight `div` 2)
