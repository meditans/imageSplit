{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Control.Lens
import           Graphics.Gloss.Interface.IO.Game hiding (text)

import           Codec.Picture.Png                (encodePng)
import           Codec.Picture.RGBA8              (readImageRGBA8, trimImage)
import           Control.Foldl                    (list)
import           Data.ByteString.Lazy             (writeFile)
import           Data.Char                        (isDigit)
import           Data.List                        (delete, minimumBy, sort, sortBy)
import           Data.Maybe                       (catMaybes, fromJust)
import           Data.Ord                         (comparing)
import           Data.Text                        (unpack)
import           Graphics.Gloss.Juicy             (loadJuicy)
import           Prelude                          hiding (writeFile)
import           Text.Read                        (readMaybe)
import           Turtle                           (pwd, ls, grep, fold, ends, format, fp)

data ImageData = ImageData { _xDim         :: Int
                           , _yDim         :: Int
                           , _fileName     :: String
                           , _cutCoords    :: [Int]
                           , _zoomLevel    :: Float
                           , _vTranslation :: Float
                           } deriving (Eq,Show)
makeLenses ''ImageData

data Image = Image { _width  :: Int
                   , _height :: Int
                   , _pic    :: Picture }
makeLenses ''Image

data AppState = AppState { _images         :: [ImageData]
                         , _currentPicture :: Picture
                         } deriving (Eq,Show)
makeLenses ''AppState

nextImage :: AppState -> IO AppState
nextImage (AppState is _) = do
  img <- fmap fromJust $ loadJuicy (head (tail is) ^. fileName)
  return $ AppState (tail is ++ [head is]) img

prevImage :: AppState -> IO AppState
prevImage (AppState is _) = do
  img <- fmap fromJust $ loadJuicy (last is ^. fileName)
  return $ AppState ([last is] ++ init is) img

main :: IO ()
main = do
  dir  <- pwd
  imgs <- flip fold list . grep (ends "png") $ format fp <$> ls dir
  app  <- fmap catMaybes . sequence . map fromFilePathToImage . sortBy (comparing page) . map unpack $ imgs
  startingImage <- fmap fromJust $ loadJuicy (head app ^. fileName)
  playIO
    (InWindow "ImageData Splitter" (1600,900) (0,0))
    white
    30
    (AppState app startingImage)
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
action (Button (SpecialKey  KeyRight)) = nextImage
action (Button (SpecialKey  KeyLeft))  = prevImage
action (Button (SpecialKey  KeyEnter)) = \s -> do
  let (ImageData x y fn cs _ _) = s ^?! images . _head
  original <- readImageRGBA8 fn
  let newCuts = sort cs
  let cutImages = map (\(a, b) -> trimImage original (x,b-a) (0,a)) $ zip (0:newCuts) (newCuts++[y])
  mapM_ (\(im, iden) -> writeFile (fn ++ "_cut" ++ show iden ++ ".png") (encodePng im)) $ zip cutImages [(1::Int)..]
  return s
action (Button (Char '+')) = return . (images . _head . zoomLevel *~  1.1)
action (Button (Char '-')) = return . (images . _head . zoomLevel //~ 1.1)
action (Button (Char '=')) = return . (images . _head . zoomLevel .~ 1)
action (Button (SpecialKey KeyUp))   = return . (images . _head . vTranslation -~ 100)
action (Button (SpecialKey KeyDown)) = return . (images . _head . vTranslation +~ 100)
action _ = return

maybeAddCut :: Float -> ImageData -> ImageData
maybeAddCut f im = over cutCoords ((if inImage f' then [f'] else []) ++) im
  where inImage x = 0 <= x && x <= (im^.yDim)
        f' = f ^. from (renderCoordY im)

deleteNear :: Int -> [Int] -> [Int]
deleteNear y xs = flip delete xs . minimumBy (comparing (abs . subtract y)) $ xs

drawState :: AppState -> IO Picture
drawState (AppState imgs pict) = do
  let im@(ImageData _ _ _ cs zm vt) = head imgs
      pict' = translate 0 vt . scale zm zm $ pict
  let cs' = map (\y -> let y' = y ^. renderCoordY im
                       in Line [(-1000, y'), (1000, y')]) cs
  return $ Pictures $ [pict'] ++ cs'

fromFilePathToImage :: String -> IO (Maybe ImageData)
fromFilePathToImage fpath = (fmap.fmap) bitmapToImage (loadJuicy fpath)
  where bitmapToImage (Bitmap x y _ _) = ImageData x y fpath [] 1 0
        bitmapToImage _ = error "This bitmap wasn't well constructed"

-- | Given an image, an isomorphism between the natural coordinates for the file
-- (with 0,0 at the top left), and the zoomed and translated coordinates for the
-- image, as used for rendering.
renderCoordX :: ImageData -> Iso' Int Float
renderCoordX (ImageData w _ _ _ zL _) = iso ((*zL) . subtract semiWeight . fromIntegral)
                                            ((/zL) ! (+ semiWeight)      ! round       )
  where semiWeight = fromIntegral w / 2
        (!) = flip (.)

renderCoordY :: ImageData -> Iso' Int Float
renderCoordY (ImageData _ h _ _ zL vT) = iso ((+ vT)        . (*zL) . (+ semiHeight)        . negate . fromIntegral)
                                             ((subtract vT) ! (/zL) ! (subtract semiHeight) ! negate ! round       )
  where semiHeight = fromIntegral h / 2
        (!) = flip (.)
