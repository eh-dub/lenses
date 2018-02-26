{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grid where

import Text.HTML.TagSoup hiding (Row, Column)

import ParseBootstrap

import Data.Traversable (for)
import Data.Maybe (fromJust)
import Data.Functor.Compose (Compose(..))
import Control.Applicative (ZipList(..))

newtype Grid a = Grid {getGrid :: Compose ZipList ZipList a}
  deriving (Functor, Applicative)

type BootstrapGrid = Grid (Int, [Tag String])
type CSS = String
type NativeGrid = Grid (CSS, [Tag String])

toGrid :: [[a]] -> Grid a
toGrid = Grid . Compose . ZipList . fmap ZipList

fromGrid :: Grid a -> [[a]]
fromGrid = fmap getZipList . getZipList . getCompose . getGrid

bootstrapToGrid :: [Tag String] -> Grid (Int, [Tag String])
bootstrapToGrid bootstrapGrid =
  toGrid $
    flip fmap (rowsFromGrid bootstrapGrid) $ \row ->
      flip fmap (columnsFromRow row) $ \column ->
        (fromJust $ columnWeight $ head column, column)

type Ids = (Int, Int)
assignIds :: Grid a -> Grid (Ids, a)
assignIds =
  overGrid (zip [0..])
           (zip [0..] . snd)
    $ \(i,row) (j, col) ->
        ((i,j), col)


incorporateIds :: Grid (Ids, [Tag String]) -> Grid [Tag String]
incorporateIds =
  overGrid id
           id
    $ \_ ((i, j), column) ->
       addId i j (head column) : tail column
       where
         -- overwriting ids is a Bad Idea
         addId i j (TagOpen tagName attrs) = TagOpen tagName $ ("id", mconcat ["blob", "-", show i, "-", show j] ) : attrs


zipGrids :: Grid a -> Grid b -> Grid (a, b)
zipGrids a b = (,) <$> a <*> b

overGrid :: ([[a]] -> [row])
         -> (row -> [col])
         -> (row -> col -> b)
         -> Grid a
         -> Grid b
overGrid rowF colF doF grid =
  toGrid $
    flip fmap (rowF $ fromGrid grid) $ \row ->
      flip fmap (colF row) $ \column ->
        doF row column

-- .grid {
--   display: grid;
  -- grid-template-rows: minmax(10px, auto);
--   grid-template-columns: Xfr, Yfr, Zfr, ...
-- }
