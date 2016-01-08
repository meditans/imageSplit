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
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import           Data.Ord                         (comparing)
import qualified Data.Ord as O                    (Down(..))
import           Data.Text                        (unpack)
import           Graphics.Gloss.Juicy             (loadJuicy)
import           Prelude                          hiding (writeFile)
import           Text.Read                        (readMaybe)
import           Turtle                           (pwd, ls, grep, fold, ends, format, fp)



--------------------------------------------------------------------------------
-- Data declarations
--------------------------------------------------------------------------------

data PictureData = PictureData { _fileName     :: String
                               , _cutCoords    :: [(Int,Int)]
                               } deriving (Eq,Show)
makeLenses ''PictureData

data AppState = AppState { _images         :: [PictureData]
                         , _currentPicture :: Picture
                         , _zoomLevel      :: Float
                         , _vTranslation   :: Float
                         , _semiCut        :: Maybe Int
                         } deriving (Eq,Show)
makeLenses ''AppState

--------------------------------------------------------------------------------
-- Main and action dispatch
--------------------------------------------------------------------------------

main :: IO ()
main = do
  dir <- pwd
  imgPaths <- sortBy (comparing page) . map unpack
          <$> (flip fold list . grep (ends "png") $ format fp <$> ls dir)
  startingImage <- fmap fromJust $ loadJuicy (head imgPaths)
  playIO
    (InWindow "PictureData Splitter" (1600,900) (0,0))
    (makeColorI 253 246 227 80)
    30
    (AppState (zipWith PictureData imgPaths (repeat [])) startingImage 1 0 Nothing)
    drawState
    action
    (\_ s -> return s)

page :: String -> Maybe Int
page = readMaybe . takeWhile (isDigit) . reverse . takeWhile (/= '/') . reverse

pattern Button b   <- EventKey b               Down _ _
pattern Click  b y <- EventKey (MouseButton b) Down (Modifiers Up Up Up) (_,y)

action :: Event -> AppState -> IO AppState
action (Click LeftButton  y) = return . maybeAddCut y
action (Click RightButton y) = return . deleteNear y
action (Button (SpecialKey  KeyRight))  = nextImage
action (Button (SpecialKey  KeyLeft))   = prevImage
action (Button (SpecialKey  KeyEnter))  = saveCuts
action (Button (SpecialKey  KeyDelete)) = deleteCuts
action (Button (Char '+'))              = return . (zoomLevel *~  1.1)
action (Button (Char '-'))              = return . (zoomLevel //~ 1.1)
action (Button (Char '='))              = return . (zoomLevel .~ 1)
action (Button (SpecialKey KeyUp))      = return . (vTranslation -~ 100)
action (Button (SpecialKey KeyDown))    = return . (vTranslation +~ 100)
action _ = return

--------------------------------------------------------------------------------
-- Functions on state
--------------------------------------------------------------------------------

nextImage :: AppState -> IO AppState
nextImage st = if length imgs < 2 then return st
  else do
    pic <- fromJust <$> loadJuicy (next ^?! _Just . fileName)
    return $ semiCut        .~ Nothing
           $ currentPicture .~ pic
           $ images         .~ (tail imgs ++ current ^.. _Just)
           $ st
  where imgs = st ^. images
        current = imgs ^? ix 0
        next    = imgs ^? ix 1

prevImage :: AppState -> IO AppState
prevImage st = if length imgs < 2 then return st
  else do
    pic <- fromJust <$> loadJuicy (prev ^?! _Just . fileName)
    return $ semiCut        .~ Nothing
           $ currentPicture .~ pic
           $ images         .~ (prev ^.. _Just ++ init imgs)
           $ st
  where imgs = st ^. images
        prev = imgs ^? reversed . ix 0

saveCuts :: AppState -> IO AppState
saveCuts st = do
  let picData = st ^?! images . _head
      cuts = sort (picData ^. cutCoords)
      w = st ^. currentPicture . to width
  original <- readImageRGBA8 (picData ^. fileName)
  let trim (a,b) = trimImage original (w,b-a) (0,a)
      cutImages  = map trim cuts
  sequence_ [ writeFile ((picData ^. fileName) ++ "_cut" ++ show iD ++ ".png") (encodePng cut)
            | (cut, iD) <- zip cutImages [(1::Int)..]]
  return st

deleteCuts :: AppState -> IO AppState
deleteCuts = return . (set semiCut Nothing) . (set (images . _head . cutCoords) [])

maybeAddCut :: Float -> AppState -> AppState
maybeAddCut y st = maybe
  (set semiCut (Just y') st)
  (\x -> set semiCut Nothing
         . over (images . _head . cutCoords) ((min x y', max x y'):)
         $ st)
  (st ^. semiCut)
  where y' = y ^. from (renderCoordY st)

deleteNear :: Float -> AppState -> AppState
deleteNear f st = over (images . ix 0 . cutCoords) (pureDeletion f') st
  where pureDeletion x xs = flip delete xs
          . minimumBy (\h k -> comparing (\(a,b) -> O.Down (btwn a b x)) h k
                            <> comparing (\(a,b) -> abs (a-x) + abs (b-x)) h k
                      ) $ xs
        f' = f ^. from (renderCoordY st)
        btwn a b x = a <= x && x <= b

drawState :: AppState -> IO Picture
drawState st = do
  let pic  = translate 0 (st ^. vTranslation)
             . scale (st ^. zoomLevel) (st ^. zoomLevel)
             $ (st ^. currentPicture)
  let cuts = map (\(a,b) -> let a' = a ^. renderCoordY st
                                b' = b ^. renderCoordY st
                            in color (makeColorI 238 232 213 110)
                               $ polygon [(-1000, a'), (1000, a'), (1000, b'), (-1000, b')])
                 (st ^. images . ix 0 . cutCoords)
  let sCut = maybe Blank (\y -> let y' = y ^. renderCoordY st
                                in line [(-1000, y'), (1000,y')])
                         (st ^. semiCut)
  return $ Pictures $ [pic] ++ cuts ++ [sCut]

--------------------------------------------------------------------------------
-- Coordinate isomorphisms
--------------------------------------------------------------------------------

-- | Given an image, an isomorphism between the natural coordinates for the file
-- (with 0,0 at the top left), and the zoomed and translated coordinates for the
-- image, as used for rendering.
renderCoordX :: AppState -> Iso' Int Float
renderCoordX st = iso ((*zL) . subtract semiWidth . fromIntegral)
                      ((/zL) ! (+ semiWidth)      ! round       )
  where semiWidth = fromIntegral (st ^. currentPicture . to width) / 2
        (!) = flip (.)
        zL = st ^. zoomLevel

renderCoordY :: AppState -> Iso' Int Float
renderCoordY st = iso ((+ vT)        . (*zL) . (+ semiHeight)        . negate . fromIntegral)
                      ((subtract vT) ! (/zL) ! (subtract semiHeight) ! negate ! round       )
  where semiHeight = fromIntegral (st ^. currentPicture . to height) / 2
        (!) = flip (.)
        vT = st ^. vTranslation
        zL = st ^. zoomLevel

width, height :: Picture -> Int
width  (Bitmap x _ _ _) = x
width  _ = error "Not a bitmap"
height (Bitmap _ y _ _) = y
height _ = error "Not a bitmap"
