module ParseBootstrap where

import Text.HTML.TagSoup

import Data.List (isPrefixOf)
import Data.Maybe (isJust)

retrieveBootstrapGrids :: String -> [[Tag String]]
retrieveBootstrapGrids html = do
  let tags = parseTags html
  let grids = sectionsNoSuffixes (\x -> x ~== TagOpen "div" [("class", "container")]) tags
  grids

rowsFromGrid :: [Tag String] -> [[Tag String]]
rowsFromGrid grid =
  sectionsNoSuffixes (\x -> x ~== TagOpen "div" [("class", "row")]) grid

columnsFromRow :: [Tag String] -> [[Tag String]]
columnsFromRow row =
  sectionsNoSuffixes isColumn row

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- supports only 'col-#' for now. Need to add support for 'col-xx-#'
columnWeight :: Tag String -> Maybe Int
columnWeight (TagOpen "div" attributes) =
  case lookup "class" attributes of
    Just class' ->
      if isPrefixOf "col" class'
        then Just $ read $ drop 4 class'
        else Nothing
    Nothing -> Nothing
columnWeight _ = Nothing

isColumn :: Tag String -> Bool
isColumn = isJust . columnWeight

-- rename later. Make it about retrieving from opening tag to its closing tag
sectionsNoSuffixes :: (Tag String -> Bool) -> [Tag String] -> [[Tag String]]
sectionsNoSuffixes f tags =
  fmap stripUnopenedTags $ sections f tags

stripUnopenedTags :: [Tag String] -> [Tag String]
stripUnopenedTags tags = fmap fst
                       $ takeWhile (\x -> (snd x) /= [])
                       $ zip tags
                       $ scannedOpenTags tags

-- remove everything
scannedOpenTags :: [Tag String] -> [[String]]
scannedOpenTags tags =
 -- replace initial empty list entry with "init" b/c empty list messes up our
 -- sick dope algo
 (["init"] :) $ tail $ scanl crushOpen [] tags
 where
   crushOpen openTags (TagOpen tag _) = tag : openTags
   crushOpen openTags (TagClose tag) = drop 1 openTags
   crushOpen openTags _ = openTags
