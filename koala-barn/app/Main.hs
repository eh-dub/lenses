module Main where

import ParseBootstrap
import Text.HTML.TagSoup

import Grid

main :: IO ()
main = do
  f <- readFile "../bootstrapStyle/index.html"
  let grids = retrieveBootstrapGrids f
  let grid1 = head grids
  -- print $ grid1
  -- putStrLn $ renderTags $ grid1
  let nativeGrid = bootstrapToGrid grid1
  -- print $ nativeGrid
  print $ fmap (renderTags . snd) <$> nativeGrid
  putStrLn $ show $ fmap (fst) <$> nativeGrid

  -- Grid with id's filled out
  print $ incorporateIds $ assignIds $ fmap snd <$> nativeGrid
