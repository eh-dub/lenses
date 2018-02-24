{-# LANGUAGE DeriveFunctor #-}

module Grid where

import Text.HTML.TagSoup hiding (Row, Column)
import ParseBootstrap

import Data.Traversable (for)
import Data.Maybe (fromJust)

-- Grid∷
--  rows
--   ratio of columns 4 4 4

data Column a =
  Column {weight::Int, contents::a}
  deriving (Show, Eq, Functor)

data Row a =
  Row [Column a]
  deriving (Show, Eq, Functor)

data Grid a =
  Grid [Row a]
  deriving (Show, Eq, Functor)

type BootstrapGrid = Grid [Tag String]

-- for
--   :: (Applicative f, Traversable t) => t a -> (a -> f b) -> f (t b)
bootstrapToGrid :: [Tag String] -> Grid ([Tag String])
bootstrapToGrid bootstrapGrid =
  Grid
  $ flip fmap (rowsFromGrid bootstrapGrid) $ \row ->
    Row $ flip fmap (columnsFromRow row) $ \column ->
      Column {weight = fromJust $ columnWeight $ head column
             ,contents = column}
