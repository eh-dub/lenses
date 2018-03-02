module Main where

import ParseBootstrap

import Text.HTML.TagSoup

import Data.Maybe (fromJust)
import Data.Monoid((<>))
import Data.Foldable(for_, traverse_)

import Grid

main :: IO [()]
main = do
  let examples = [1,2,3]
  sequence $ fmap (convertExample . show) examples

convertExample :: String -> IO ()
convertExample folder = do
  f <- readFile $ "../bootstrapExamples/" <> folder <> "/index.html"
  let grids = retrieveBootstrapGrids f


  let convertedGrids = fmap bootstrapToGrid grids


  let grid1 = head grids
  -- print $ grid1
  -- putStrLn $ renderTags $ grid1
  let tagGrid = bootstrapToGrid grid1
  print tagGrid
  let idGrid = fillInIds tagGrid

  writeFile ("../codeGen/" <> folder <> "/index.html") $ pageHTML convertedGrids
  writeFile ("../codeGen/" <> folder <> "/style.css") $ pageCSS convertedGrids 
