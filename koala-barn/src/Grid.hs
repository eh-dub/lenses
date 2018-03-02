{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Grid where

import Text.HTML.TagSoup hiding (Row, Column)

import ParseBootstrap

import Data.Traversable (sequenceA)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Functor.Compose (Compose(..))
import Control.Applicative (ZipList(..))

newtype Grid a = Grid {getGrid :: Compose ZipList ZipList a}
  deriving (Functor, Applicative, Foldable, Traversable)

instance Show a => Show (Grid a) where
  show = show . fromGrid

data HTMLState = NoIds | HasIds

newtype HTML (s :: HTMLState) = HTML {getHTML :: [Tag String]}
  deriving (Eq, Ord, Show)

type BootstrapGrid = Grid (Int, [Tag String])
type CSS = String
type NativeGrid = Grid (CSS, [Tag String])

toGrid :: [[a]] -> Grid a
toGrid = Grid . Compose . ZipList . fmap ZipList

fromGrid :: Grid a -> [[a]]
fromGrid = fmap getZipList . getZipList . getCompose . getGrid

bootstrapToGrid :: [Tag String] -> Grid (HTML NoIds)
bootstrapToGrid =
  fmap HTML . toGrid . fmap columnsFromRow . rowsFromGrid

toWeightGrid :: Grid (HTML s) -> Maybe (Grid Int)
toWeightGrid = traverse $ columnWeight . head . getHTML

-- This is dumb. grid spans should be done at the CSS library level.
-- Should instead worry about sucking up additional CSS
toCSSGrid :: Grid (HTML HasIds) -> Maybe (Grid CSS)
toCSSGrid g = do
  weightGrid <- toWeightGrid g
  sequenceA $ pure f <*> g <*> weightGrid
  where
    f html weight = do
      TagOpen tagName attrs <- pure $ head $ getHTML html
      id' <- lookup "id" attrs
      pure $ mconcat [ "#", id', "{\n"
              , "\tgrid-column: span ", show weight, ";\n}\n" ]

infiniteGrid :: Grid (Int, Int)
infiniteGrid =
  toGrid
    $ let makeRow r = fmap (r ,) [0..]
      in
        fmap makeRow [0..]

fillInIds :: Grid (HTML NoIds) -> Grid (HTML HasIds)
fillInIds tags =
  fmap HTML
    $ pure updateTag <*> infiniteGrid <*> fmap getHTML tags
  where
    updateTag (i, j) column = addId i j (head column) : tail column
    addId i j (TagOpen tagName attrs) =
      TagOpen tagName
        -- overwriting ids is a Bad Idea®
        $ ("id", mconcat ["col", "-", show i, "-", show j] ) : attrs

zipGrids :: Grid a -> Grid b -> Grid (a, b)
zipGrids a b = (,) <$> a <*> b

pageHTML :: [Grid (HTML a)] -> String
pageHTML grids = mconcat $ fmap (<> "\n") [ "<!DOCTYPE HTML>"
                   , "<html>"
                   , "<head>"
                   , "<link rel=\"stylesheet\" href=\"../bootstrapInCssGrid.css\">"
                   , "<link rel=\"stylesheet\" href=\"./style.css\">"
                   , "</head>"
                   , "<body>"
                   , mconcat $ (fmap gridHTML $ zip grids [0..])
                   , "</body>"
                   , "</html>"
                   ]

gridHTML :: (Grid (HTML a), Int) -> String
gridHTML (g, id') =
  mconcat [ "<div class=\"grid\" id=\"grid-" <> show id' <> "\">\n"
          , foldMap ((<> "\n") . renderTags) $ fmap getHTML g
          , "</div>"
          ]

pageCSS :: [Grid (HTML a)] -> String
pageCSS grids = mconcat $ fmap gridCSS $ zip grids [0..]

-- @TODO: Compute number of rows in grid
gridCSS :: (Grid (HTML a), Int) -> String
gridCSS (g, id') =
  let numRows = show $ length $ fromGrid g
  in
    mconcat [ "#grid-"
            , show id'
            , " {\n\tdisplay: grid;"
            , "\n\tgrid-template-rows: "
            , "repeat(" <> numRows <> ", minmax(10px, auto));}\n"
            ]
