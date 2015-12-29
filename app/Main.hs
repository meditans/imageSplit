{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Control.Lens
import           Graphics.Gloss.Interface.IO.Game hiding (text)
import           Turtle

import           Codec.Picture.Png                (encodePng)
import           Codec.Picture.RGBA8              (readImageRGBA8, trimImage)
import           Control.Foldl                    (list)
import           Data.ByteString.Lazy             (writeFile)
import           Data.Char                        (isDigit)
import           Data.List                        (delete, minimumBy, sort, sortBy)
import           Data.Maybe                       (catMaybes, fromJust, isJust)
import           Data.Ord                         (comparing)
import           Data.Text                        (unpack)
import           Graphics.Gloss.Juicy             (loadJuicy)
import           Prelude                          hiding (writeFile)
import           Text.Read                        (readMaybe)

data Image = Image { _xDim         :: Int
                   , _yDim         :: Int
                   , _fileName     :: String
                   , _imageBMP     :: Picture
                   , _cutCoords    :: [Int]
                   , _zoomLevel    :: Float
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
  imgs <- flip fold list . grep (ends "png") $ format fp <$> ls dir
  app  <- fmap catMaybes . sequence . map fromFilePathToImage . sortBy (comparing page) . map unpack $ imgs
  playIO
    (InWindow "Image Splitter" (1600,900) (0,0))
    white
    30
    (AppState app)
    drawState
    action
    (\_ s -> return s)

page :: String -> Maybe Int
page = readMaybe . takeWhile (isDigit) . reverse . takeWhile (/= '/') . reverse

pattern Button b   <- EventKey b               Down _ _
pattern Click  b y <- EventKey (MouseButton b) Down (Modifiers Up Up Up) (_,y)

action :: Event -> AppState -> IO AppState
action (Click LeftButton  y) = return . (images . _head %~ maybeAddCut y)
action (Click RightButton y) = \s ->
  return . (images . _head . cutCoords %~ deleteNear (y ^. from (renderCoordY (s^?!images._head)))) $ s
action (Button (SpecialKey  KeyRight)) = return . nextImage
action (Button (SpecialKey  KeyLeft))  = return . prevImage
action (Button (SpecialKey  KeyEnter)) = \s -> do
  let (Image x y fn _ cs _ _) = s ^?! images . _head
  original <- readImageRGBA8 fn
  let newCuts = sort cs
  let cutImages = map (\(a, b) -> trimImage original (x,b-a) (0,a)) $ zip (0:newCuts) (newCuts++[y])
  mapM_ (\(im, iden) -> writeFile (fn ++ "_cut" ++ show iden ++ ".png") (encodePng im)) $ zip cutImages [1..]
  return s
action (Button (Char '+')) = return . (images . _head . zoomLevel *~  1.1)
action (Button (Char '-')) = return . (images . _head . zoomLevel //~ 1.1)
action (Button (Char '=')) = return . (images . _head . zoomLevel .~ 1)
action (Button (SpecialKey KeyUp)) = return . (images . _head . vTranslation -~ 100)
action (Button (SpecialKey KeyDown)) = return . (images . _head . vTranslation +~ 100)
action _ = return

maybeAddCut :: Float -> Image -> Image
maybeAddCut f im = over cutCoords ((if inImage f' then [f'] else []) ++) im
  where inImage x = 0 <= x && x <= (im^.yDim)
        f' = f ^. from (renderCoordY im)

deleteNear :: Int -> [Int] -> [Int]
deleteNear y xs = flip delete xs . minimumBy (comparing (abs . subtract y)) $ xs

drawState :: AppState -> IO Picture
drawState (AppState imgs) = do
  let im@(Image _ h fn dt cs zm vt) = head imgs
      pic = translate 0 vt . scale zm zm $ dt
  let cs' = map (\y -> let y' = y ^. renderCoordY im
                       in Line [(-1000, y'), (1000, y')]) cs
  return $ Pictures $ [pic] ++ cs'

fromFilePathToImage :: String -> IO (Maybe Image)
fromFilePathToImage fp = (fmap.fmap) (\im -> let (Bitmap x y _ _) = im in Image x y fp im [] 1 0) (loadJuicy fp)

-- | Given an image, an isomorphism between the natural coordinates for the file
-- (with 0,0 at the top left), and the zoomed and translated coordinates for the
-- image, as used for rendering.
renderCoordX :: Image -> Iso' Int Float
renderCoordX (Image w _ _ _ _ zL _) = iso ((*zL) . subtract semiWeight . fromIntegral)
                                          ((/zL) ! (+ semiWeight)      ! round       )
  where semiWeight = fromIntegral w / 2
        (!) = flip (.)

renderCoordY :: Image -> Iso' Int Float
renderCoordY (Image _ h _ _ _ zL vT) = iso ((+ vT)        . (*zL) . (+ semiHeight)        . negate . fromIntegral)
                                           ((subtract vT) ! (/zL) ! (subtract semiHeight) ! negate ! round       )
  where semiHeight = fromIntegral h / 2
        (!) = flip (.)
