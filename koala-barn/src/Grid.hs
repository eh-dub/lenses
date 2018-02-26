{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

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

type BootstrapGrid = Grid (Int, [Tag String])
type CSS = String
type NativeGrid = Grid (CSS, [Tag String])

toGrid :: [[a]] -> Grid a
toGrid = Grid . Compose . ZipList . fmap ZipList

fromGrid :: Grid a -> [[a]]
fromGrid = fmap getZipList . getZipList . getCompose . getGrid

bootstrapToTagGrid :: [Tag String] -> Grid [Tag String]
bootstrapToTagGrid =
  toGrid . fmap columnsFromRow . rowsFromGrid

toWeightGrid :: Grid [Tag String] -> Maybe (Grid Int)
toWeightGrid = traverse $ columnWeight . head

infiniteGrid :: Grid (Int, Int)
infiniteGrid =
  toGrid
    $ let makeRow r = fmap (r ,) [0..]
      in
        fmap makeRow [0..]

incorporateIds :: Grid [Tag String] -> Grid [Tag String]
incorporateIds tags =
  pure updateTag <*> infiniteGrid <*> tags
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
