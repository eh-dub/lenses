module Lib where

import Text.HTML.TagSoup

retrieveBootstrapGrids :: String -> [[Tag String]]
retrieveBootstrapGrids html = do
  let tags = parseTags html
  let grids = sectionsNoSuffixes (\x -> x ~== TagOpen "div" [("class", "container")]) tags
  grids

-- rename later. Make it about retrieving from opening tag to its closing tag
sectionsNoSuffixes :: (Tag String -> Bool) -> [Tag String] -> [[Tag String]]
sectionsNoSuffixes f tags =
  fmap stripUnopenedTags $ sections f tags

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

stripUnopenedTags :: [Tag String] -> [Tag String]
stripUnopenedTags tags = fmap fst $ takeWhile (\x -> (snd x) /= []) $
  zip tags (scannedOpenTags tags)
