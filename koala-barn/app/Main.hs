module Main where

import ParseBootstrap

import Text.HTML.TagSoup

import Data.Maybe (fromJust)


import Grid

main :: IO ()
main = do
  f <- readFile "../bootstrapStyle/index.html"
  let grids = retrieveBootstrapGrids f
  let grid1 = head grids
  -- print $ grid1
  -- putStrLn $ renderTags $ grid1
  let tagGrid = bootstrapToGrid grid1
  let idGrid = fillInIds tagGrid
  let weightGrid = fromJust $ toWeightGrid idGrid


  putStrLn "\n"
  print tagGrid
  putStrLn "\n"
  print idGrid
  putStrLn "\n"
  print weightGrid
