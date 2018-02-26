module Main where

import ParseBootstrap
import Text.HTML.TagSoup

import Grid

main :: IO ()
main = do
  f <- readFile "../bootstrapStyle/index.html"
  let grids = retrieveBootstrapGrids f
  let grid1 = head grids
  -- putStrLn $ show $ grid1
  -- putStrLn $ renderTags $ grid1
  let nativeGrid = bootstrapToGrid grid1
  -- putStrLn $ show $ nativeGrid
  putStrLn $ show $ fmap (renderTags . snd) <$> nativeGrid
  putStrLn $ show $ fmap (fst) <$> nativeGrid
  putStrLn $ show $ assignIds nativeGrid
