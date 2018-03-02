module Main where

import ParseBootstrap

import Text.HTML.TagSoup

import Data.Maybe (fromJust)
import Data.Monoid((<>))
import Data.Foldable(for_, traverse_)

import Grid

main :: IO ()
main = do
  f <- readFile "../bootstrapExamples/1/index.html"
  let grids = retrieveBootstrapGrids f
  let grid1 = head grids
  -- print $ grid1
  -- putStrLn $ renderTags $ grid1
  let tagGrid = bootstrapToGrid grid1
  let idGrid = fillInIds tagGrid

  writeFile "../codeGen/index.html" $ pageHTML idGrid
  writeFile "../codeGen/style.css" $ gridCSS ".grid"

  -- traverse_ (writeFile "../codeGen/style.css") $ do
  --   cssGrid <- toCSSGrid idGrid
  --   pure $ gridCSS ".grid" <> foldMap id cssGrid

  putStrLn "\n"
  print tagGrid
  putStrLn "\n"
  print idGrid
  putStrLn "\n"
