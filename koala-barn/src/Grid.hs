{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Grid where

import Text.HTML.TagSoup hiding (Row, Column)

import ParseBootstrap

import Data.Traversable (sequenceA)
import Data.Maybe (fromJust)
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

toCSSGrid :: Grid (HTML HasIds) -> Maybe (Grid CSS)
toCSSGrid g = do
  weightGrid <- toWeightGrid g
  Just $ pure f <*> (fmap (head . getHTML) g) <*> weightGrid
  where
    f (TagOpen tagName attrs) weight =
      mconcat [ "#", fromJust $ lookup "id" attrs, "{\n"
              , "\tgrid-column: span ", show weight, ";\n}\n" ]

    -- lookup :: Eq a => a -> [(a, b)] -> Maybe b

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

-- .grid {
--   display: grid;
  -- grid-template-rows: minmax(10px, auto);
--   grid-template-columns: Xfr, Yfr, Zfr, ...
-- }
