{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Control.Lens
import           Graphics.Gloss.Interface.IO.Game hiding (text)

import           Codec.Picture.Png                (encodePng, writePng)
import           Codec.Picture.RGBA8              (patchImage, readImageRGBA8,
                                                   trimImage)
import           Codec.Picture.Types              (Image (..), PixelRGBA8 (..),
                                                   generateImage)
import           Control.Foldl                    (list)
import           Data.ByteString.Lazy             (writeFile)
import           Data.Char                        (isDigit)
import           Data.List                        (foldl1', sort, sortBy)
import           Data.Monoid                      ((<>))
import           Data.Ord                         (comparing)
import qualified Data.Ord                         as O (Down (..))
import           Data.Text                        (unpack)
import           Graphics.Gloss.Juicy             (fromImageRGBA8)
import           Prelude                          hiding (writeFile)
import           Text.Read                        (readMaybe)
import           Turtle                           (ends, fold, format, fp, grep,
                                                   ls, pwd)

import           MultiCut

--------------------------------------------------------------------------------
-- Data declarations
--------------------------------------------------------------------------------

data AppState = AppState { _fileNames      :: [String]
                         , _currentImage   :: Image PixelRGBA8
                         , _currentPicture :: Picture
                         , _zoomLevel      :: Float
                         , _vTranslation   :: Float
                         , _semiCut        :: Maybe Int
                         , _multiCuts      :: [MultiCut]
                         }
makeLenses ''AppState

--------------------------------------------------------------------------------
-- Main and action dispatch
--------------------------------------------------------------------------------

main :: IO ()
main = do
  dir <- pwd
  imgPaths <- sortBy (comparing page) . map unpack
          <$> (flip fold list . grep (ends "png") $ format fp <$> ls dir)
  startingImage <- readImageRGBA8 (head imgPaths)
  playIO
    (InWindow "PictureData Splitter" (1600,900) (0,0))
    (makeColorI 253 246 227 80)
    30
    (AppState imgPaths startingImage (fromImageRGBA8 startingImage) 1 0 Nothing [])
    drawState
    action
    (\_ s -> return s)

page :: String -> Maybe Int
page = readMaybe . takeWhile (isDigit) . reverse . takeWhile (/= '/') . reverse

pattern Button      b   <- EventKey b               Down _ _
pattern Click       b y <- EventKey (MouseButton b) Down (Modifiers Up   Up Up) (_,y)
pattern ShiftClick  b y <- EventKey (MouseButton b) Down (Modifiers Down Up Up) (_,y)

action :: Event -> AppState -> IO AppState
action (Click LeftButton  y)            = return . maybeAddCut y
action (Click RightButton y)            = return . deleteNear y
action (ShiftClick LeftButton y)        = return . selectNear y
action (Button (SpecialKey  KeyRight))  = nextImage
action (Button (SpecialKey  KeyLeft))   = prevImage
action (Button (SpecialKey  KeyEnter))  = saveCuts
action (Button (SpecialKey  KeyDelete)) = deleteCuts
action (Button (Char 'd'))              = dualPage
action (Button (Char 'm'))              = return . (multiCuts %~ mergeSelected)
action (Button (Char '+'))              = return . (zoomLevel *~  1.1)
action (Button (Char '-'))              = return . (zoomLevel //~ 1.1)
action (Button (Char '='))              = return . (zoomLevel .~ 1)
action (Button (SpecialKey KeyUp))      = return . (vTranslation -~ 100)
action (Button (SpecialKey KeyDown))    = return . (vTranslation +~ 100)
action _ = return

--------------------------------------------------------------------------------
-- Functions on state
--------------------------------------------------------------------------------

maybeAddCut :: Float -> AppState -> AppState
maybeAddCut y st = maybe
  (set semiCut (Just y') st)
  (\x -> set semiCut Nothing
         . over multiCuts (MultiCut [(min x y', max x y')] False :)
         $ st)
  (st ^. semiCut)
  where y' = y ^. from (renderCoordY st)

deleteNear :: Float -> AppState -> AppState
deleteNear y' st = (multiCuts %~ filter (not . null . view cuts)
                               . map (maybe id deleteCut near)) st
  where near = cutNear y' st

selectNear :: Float -> AppState -> AppState
selectNear y' st = maybe st (\c -> multiCuts %~ selectContaining c $ st) (cutNear y' st)

nextImage :: AppState -> IO AppState
nextImage st = if length imgs < 2 then return st
  else do
    pic <- readImageRGBA8 next
    return $ semiCut        .~ Nothing
           $ currentPicture .~ fromImageRGBA8 pic
           $ currentImage   .~ pic
           $ fileNames      .~ (tail imgs ++ [current])
           $ st
  where imgs = st ^. fileNames
        current = imgs ^. ix 0
        next    = imgs ^. ix 1

prevImage :: AppState -> IO AppState
prevImage st = if length imgs < 2 then return st
  else do
    pic <- readImageRGBA8 prev
    return $ semiCut        .~ Nothing
           $ currentPicture .~ fromImageRGBA8 pic
           $ currentImage   .~ pic
           $ fileNames      .~ (prev : init imgs)
           $ st
  where imgs = st ^. fileNames
        prev = imgs ^. reversed . ix 0

saveCuts :: AppState -> IO AppState
saveCuts st = do
  let picName = st ^?! fileNames . _head
      mcuts = sort (st ^. multiCuts)
  let original = st ^. currentImage
  let cutImages  = map (multiTrim original) mcuts
  sequence_ [ writeFile (picName ++ "_cut" ++ show iD ++ ".png") (encodePng cut)
            | (cut, iD) <- zip cutImages [(1::Int)..]]
  return st

deleteCuts :: AppState -> IO AppState
deleteCuts = return . (set semiCut Nothing) . (multiCuts .~ [])

dualPage :: AppState -> IO AppState
dualPage st
  | length (st ^. fileNames) < 2 = return st
  | otherwise = deleteCuts st >>= (\nst -> do
      im1 <- readImageRGBA8 (nst ^. fileNames . ix 0)
      im2 <- readImageRGBA8 (nst ^. fileNames . ix 1)
      let im = mergePages im1 im2
      return . (currentPicture .~ fromImageRGBA8 im) . (currentImage .~ im) $ nst)

--------------------------------------------------------------------------------
-- Localize the nearest cut
--------------------------------------------------------------------------------

cutNear :: Float -> AppState -> Maybe Cut
cutNear y' st = minimumByOf (multiCuts . folded . cuts . folded) comparison st
  where
    comparison c1 c2 = comparing (\(a,b) -> O.Down (btwn a b y))   c1 c2
                    <> comparing (\(a,b) -> abs (a-y) + abs (b-y)) c1 c2
    y = y' ^. from (renderCoordY st)
    btwn a b x = a <= x && x <= b

--------------------------------------------------------------------------------
-- Drawing functions
--------------------------------------------------------------------------------

drawState :: AppState -> IO Picture
drawState st = do
  let pic  = translate 0 (st ^. vTranslation)
             . scale (st ^. zoomLevel) (st ^. zoomLevel)
             $ (st ^. currentPicture)
  let cs   = map (drawMultiCut st) (st ^. multiCuts)
  let sCut = maybe Blank (\y -> let y' = y ^. renderCoordY st
                                in line [(-1000, y'), (1000,y')])
                         (st ^. semiCut)
  return $ Pictures $ [pic] ++ cs ++ [sCut]

drawMultiCut :: AppState -> MultiCut -> Picture
drawMultiCut st mc = Pictures $ map horizontalRectangle (mc ^. cuts) ++ [verticalRectangle]
  where
    horizontalRectangle (a,b) = let a' = a ^. renderCoordY st
                                    b' = b ^. renderCoordY st
                                in color (makeColorI 238 232 213 110)
                                   $ polygon [(-1000, a'), (1000, a'), (1000, b'), (-1000, b')]
    verticalRectangle = let c = minimumOf (cuts . folded . _1) mc ^?! _Just
                            d = maximumOf (cuts . folded . _2) mc ^?! _Just
                            (Bitmap w _ _ _) = st ^. currentImage . to fromImageRGBA8
                            c' = c ^. renderCoordY st
                            d' = d ^. renderCoordY st
                            w' = fromIntegral w
                        in color (if (mc^.selected) then makeColorI 255 0 0 255 else makeColorI 50 50 50 255)
                           $ polygon [(w'-250, c'), (w'-225, c'), (w'-225, d'), (w'-250, d')]

--------------------------------------------------------------------------------
-- Coordinate isomorphisms
--------------------------------------------------------------------------------

-- | Given an image, an isomorphism between the natural coordinates for the file
-- (with 0,0 at the top left), and the zoomed and translated coordinates for the
-- image, as used for rendering.
renderCoordX :: AppState -> Iso' Int Float
renderCoordX st = iso ((*zL) . subtract semiWidth . fromIntegral)
                      ((/zL) ! (+ semiWidth)      ! round       )
  where semiWidth = fromIntegral (st ^. currentImage . to fromImageRGBA8 . to width) / 2
        (!) = flip (.)
        zL = st ^. zoomLevel

renderCoordY :: AppState -> Iso' Int Float
renderCoordY st = iso ((+ vT)        . (*zL) . (+ semiHeight)        . negate . fromIntegral)
                      ((subtract vT) ! (/zL) ! (subtract semiHeight) ! negate ! round       )
  where semiHeight = fromIntegral (st ^. currentImage . to fromImageRGBA8 . to height) / 2
        (!) = flip (.)
        vT = st ^. vTranslation
        zL = st ^. zoomLevel

width, height :: Picture -> Int
width  (Bitmap x _ _ _) = x
width  _ = error "Not a bitmap"
height (Bitmap _ y _ _) = y
height _ = error "Not a bitmap"

--------------------------------------------------------------------------------
-- Page merging
--------------------------------------------------------------------------------

mergePages :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
mergePages im1@(Image w1 h1 _) im2@(Image w2 h2 _) =
    patch (nx1, ny1) im1
  $ patch (nx2, ny2) im2
  $ generateImage (const . const $ whitePixel) (max w1 w2) (h1 + h2)
  where
    whitePixel = PixelRGBA8 255 255 255 255
    patch pos target source = patchImage source pos target
    (nx1, ny1) = if w1 >= w2 then (0,0) else ((w1-w2) `div` 2, 0)
    (nx2, ny2) = if w1 >= w2 then ((w1-w2) `div` 2, h1) else (0,h1)

testMergePages :: IO ()
testMergePages = do
 p1 <- readImageRGBA8 "page1.png"
 p2 <- readImageRGBA8 "page2.png"
 let p3 = mergePages p1 p2
 writePng "page3.png" p3

multiTrim :: Image PixelRGBA8 -> MultiCut -> Image PixelRGBA8
multiTrim im@(Image w _ _) = foldl1' mergePages . map (\(a,b) -> trimImage im (w,b-a) (0,a)) . sort . view cuts
