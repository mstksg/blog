module Web.Blog.Models.EntryI where

import Control.Applicative
import Control.Monad.Reader
import Data.List                             (find, sortBy)
import Data.Maybe                            (fromJust, catMaybes)
import Data.Ord                              (comparing)
import Data.Time
import Web.Blog.Models.Entry                 (groupEntries)
import Web.Blog.Models.Models
import Web.Blog.Models.Types
import Web.Blog.Render
import Web.Blog.Types
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

postedEntriesI :: UTCTime -> RouteReaderM (KeyMap Entry)
postedEntriesI now =
  M.filter (F.any (< now) . entryPostedAt) . siteDatabaseEntries <$> askDb

postedEntryCountI :: UTCTime -> RouteReaderM Int
postedEntryCountI = (M.size <$>) . postedEntriesI

getEntryDataI :: KeyMapKey Entry -> RouteReaderM (T.Text,[Tag])
getEntryDataI k = (,) <$> getUrlPathI k <*> getTagsI k

wrapEntryDataI :: KeyMapKey Entry -> RouteReaderM (KeyMapPair Entry, (T.Text, [Tag]))
wrapEntryDataI k = do
  eData   <- getEntryDataI k
  e       <- fromJust . (k `M.lookup`) . siteDatabaseEntries <$> askDb
  return ((k, e), eData)

getTagsI :: KeyMapKey Entry -> RouteReaderM [Tag]
getTagsI k = do
  entryTags <- M.elems . siteDatabaseEntryTags <$> askDb
  let
    filtered = filter ((== k) . entryTagEntryId) entryTags
    tagKeys = map entryTagTagId filtered

  maybeTags <- forM tagKeys $ \tk ->
    M.lookup tk . siteDatabaseTags <$> askDb

  let
    tags = catMaybes maybeTags
    tagsOf tt = filter ((== tt) . tagType_) tags

  return $ concatMap tagsOf [GeneralTag ..]


getUrlPathI :: KeyMapKey Entry -> RouteReaderM T.Text
getUrlPathI k = do
  currSlug <- getCurrentSlugI k
  return $ case currSlug of
    Just slug -> T.append "/entry/" (slugSlug slug)
    Nothing -> T.append "/entry/id/" (T.pack . show . intFromKey $ k)

intFromKey :: KeyMapKey a -> Int
intFromKey (D.Key (D.PersistInt64 i)) = fromIntegral i
intFromKey _ = undefined

getCurrentSlugI :: KeyMapKey Entry -> RouteReaderM (Maybe Slug)
getCurrentSlugI k =
  find ((&&) <$> slugIsCurrent <*> matchingSlug) .
    M.elems . siteDatabaseSlugs <$> askDb
  where
    matchingSlug = (== k) . slugEntryId

groupEntriesI :: [KeyMapPair Entry] -> [[[KeyMapPair Entry]]]
groupEntriesI = (map . map . map) entityToPair . groupEntries . map pairToEntity

pairToEntity :: KeyMapPair Entry -> D.Entity Entry
pairToEntity = undefined

entityToPair :: D.Entity Entry -> KeyMapPair Entry
entityToPair = undefined

keyToPair :: KeyMapKey Entry -> RouteReaderM (Maybe (KeyMapPair Entry))
keyToPair k = do
  entries <- siteDatabaseEntries <$> askDb
  return $ (,) k <$> k `M.lookup` entries

sortEntriesI :: [Entry] -> [Entry]
sortEntriesI = sortBy (comparing entryPostedAt)

sortEntryKeysI :: [KeyMapKey Entry] -> RouteReaderM [KeyMapKey Entry]
sortEntryKeysI ks = do
  entries <- siteDatabaseEntries <$> askDb
  return $ sortBy (comparing (keyPostedAt entries)) ks
  where
    keyPostedAt es k = k `M.lookup` es >>= entryPostedAt


