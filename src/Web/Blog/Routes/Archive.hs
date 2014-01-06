module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveTag
  , routeArchiveYear
  , routeArchiveMonth
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe                            (listToMaybe, catMaybes)
import Data.Time
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.EntryI
import Web.Blog.Models.Types
import Control.Applicative ((<$>))

import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Archive
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

-- routeArchive :: T.Text -> [KeyMapPair Entry] -> ViewArchiveType -> RouteDatabase
-- routeArchive = ((return .) .) . readerArchive
  -- let
  --   grouped = groupEntries entries

  -- eList' <- liftIO $ runDB $ mapM (mapM (mapM wrapEntryData)) grouped
  -- blankPageData <- genPageData

  -- let
  --   view = viewArchive eList' viewType
  --   pageData = blankPageData { pageDataTitle = Just title
  --                            , pageDataCss   = ["/css/page/archive.css"]
  --                            , pageDataJs    = ["/js/disqus_count.js"] }

  -- return $ siteRight (view, pageData)

readerArchive :: T.Text -> [KeyMapPair Entry] -> ViewArchiveType -> RouteReader
readerArchive title entries viewType = do
  (_,blankPageData) <- ask

  let
    grouped = groupEntriesI entries
    -- groupedKeys = (map . map . map) fst grouped

  eList <- (mapM . mapM . mapM) (wrapEntryDataI . fst) grouped

  let
    view = viewArchive eList viewType
    pageData = blankPageData { pageDataTitle = Just title
                             , pageDataCss   = ["/css/page/archive.css"]
                             , pageDataJs    = ["/js/disqus_count.js"] }

  siteRight (view, pageData)

-- routeArchiveFilters :: T.Text -> [D.Filter Entry] -> ViewArchiveType -> RouteDatabase
-- routeArchiveFilters title filters pdMap = do
--   entries <- liftIO $ runDB $
--     postedEntriesFilter filters [ D.Desc EntryPostedAt ]
--   routeArchive title entries pdMap

routeArchiveAll :: RouteDatabase
routeArchiveAll = do
  now <- liftIO getCurrentTime
  return $ readerArchiveAll now
  -- routeArchiveFilters "History" [] ViewArchiveAll

readerArchiveAll :: UTCTime -> RouteReader
readerArchiveAll now = do
  es <- postedEntriesI now
  readerArchive "History" (M.toList es) ViewArchiveAll

-- routeArchiveTag :: TagType -> T.Text -> RouteDatabase
-- routeArchiveTag type_ slug = do
--   tag <- liftIO $ runDB $ D.getBy $ UniqueSlugType slug type_

--   case tag of
--     Just (D.Entity tagKey tag') -> do
--       entrytags <- liftIO $ runDB $ D.selectList [ EntryTagTagId D.==. tagKey ] []
--       let
--         entryKeys = map (entryTagEntryId . D.entityVal) entrytags
--         viewType = case type_ of
--                 GeneralTag  -> ViewArchiveTag
--                 CategoryTag -> ViewArchiveCategory
--                 SeriesTag   -> ViewArchiveSeries

--       routeArchiveFilters (tagLabel' tag') [ EntryId D.<-. entryKeys ] $ viewType tag'

--     Nothing ->
--       return $ error404 "TagNotFound"

routeArchiveTag :: TagType -> T.Text -> RouteDatabase
routeArchiveTag type_ slug = do
  now <- liftIO getCurrentTime
  return $ readerArchiveTag now type_ slug

readerArchiveTag :: UTCTime -> TagType -> T.Text -> RouteReader
readerArchiveTag now type_ slug = do
  tags <- siteDatabaseTags <$> askDb
  let
    matchSlug = (== slug) . tagSlug 
    tag = listToMaybe . M.toList $ M.filter matchSlug tags

  case tag of
    Just (tagKey, tag') -> do

      entryTags <- do
        ets <- siteDatabaseEntryTags <$> askDb
        let
          matchEntryTag = (== tagKey) . entryTagTagId
          entryTags = M.elems $ M.filter matchEntryTag ets
          entryKeys = map entryTagEntryId entryTags

        alls <- forM entryKeys $ \k ->
          (k `M.lookup`) . siteDatabaseEntryTags <$> askDb

        let
          entries = catMaybes alls
          viewType = case type_ of
                  GeneralTag  -> ViewArchiveTag
                  CategoryTag -> ViewArchiveCategory
                  SeriesTag   -> ViewArchiveSeries

      

        readerArchive (tagLabel' tag') entries (viewType tag')

        

      error404 "ey"

    Nothing -> error404 "TagNotFound"
  

  siteLeft ""

-- routeArchiveYear :: Int -> RouteDatabase
-- routeArchiveYear year = routeArchiveFilters (T.pack $ show year) filters $ ViewArchiveYear year
--   where
--     startTime = buildTime defaultTimeLocale [('Y',show year)] :: UTCTime
--     endTime = buildTime defaultTimeLocale [('Y',show $ year + 1)] :: UTCTime
--     filters = [ EntryPostedAt D.>=. Just startTime
--               , EntryPostedAt D.<=. Just endTime  ]

-- routeArchiveMonth :: Int -> Int -> RouteDatabase
-- routeArchiveMonth year month = routeArchiveFilters (T.pack timeString) filters $ ViewArchiveMonth year month
--   where
--     startDay = buildTime defaultTimeLocale
--       [('Y',show year),('m',show month)] :: Day
--     endDay = addGregorianMonthsRollOver 1 startDay
--     startTime = UTCTime startDay 0
--     endTime = UTCTime endDay 0
--     filters = [ EntryPostedAt D.>=. Just startTime
--               , EntryPostedAt D.<=. Just endTime  ]
--     timeString = formatTime defaultTimeLocale "%B %Y" startDay

