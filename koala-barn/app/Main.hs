module Main where

import Lib
import Text.HTML.TagSoup

main :: IO ()
main = do
  f <- readFile "../bootstrapStyle/index.html"
  let grids = retrieveBootstrapGrids f
  let grid1 = head grids
  putStrLn $ show $ grid1
  putStrLn $ renderTags $ grid1
-- main = void retrieveBootstrapGrids
