{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Text.HTML.TagSoup hiding (Row, Column)

import ParseBootstrap

import Data.Traversable (for)
import Data.Maybe (fromJust)
import Data.Functor.Compose (Compose(..))
import Control.Applicative (liftA2)

type Grid a = [[a]]

type BootstrapGrid = Grid (Int, [Tag String])
type CSS = String
type NativeGrid = Grid (CSS, [Tag String])

bootstrapToGrid :: [Tag String] -> Grid (Int, [Tag String])
bootstrapToGrid bootstrapGrid =
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

fGrid :: (a -> b) -> Grid a -> Grid b
fGrid f =
  getCompose . fmap f . Compose

zipGrids :: Grid a -> Grid b -> Grid (a, b)
zipGrids a b = getCompose $ pure (,) <*> Compose a <*> Compose b

appGrid :: Grid (a -> b) -> Grid a -> Grid b
appGrid f as =
   getCompose $ Compose f <*> Compose as
  -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c

overGrid :: (Grid a -> [row])
         -> (row -> [col])
         -> (row -> col -> b)
         -> Grid a
         -> Grid b
overGrid rowF colF doF grid =
  flip fmap (rowF grid) $ \row ->
    flip fmap (colF row) $ \column ->
      doF row column

-- .grid {
--   display: grid;
  -- grid-template-rows: minmax(10px, auto);
--   grid-template-columns: Xfr, Yfr, Zfr, ...
-- }
