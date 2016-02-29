{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- import           Data.Traversable
-- import           Text.Sass
import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util
import           Blog.View
import           Blog.View.Home
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time.Format
import           Data.Time.LocalTime
import           Hakyll
import           Hakyll.Web.Blaze
import           Hakyll.Web.Redirect
import           Hakyll.Web.Sass
import           System.FilePath
import           Text.Jasmine
import           Text.Read                 (readMaybe)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.Yaml                 as Y
import qualified Text.Pandoc               as P
import qualified Text.Pandoc.Walk          as P

main :: IO ()
main = do
    c@Config{..} <- either throwIO return
                =<< Y.decodeFileEither "config/site-data.yaml"
    let ?config = c

    lTempl <- readFile "latex/templates/default.latex"

    hakyll $ do
      match "static/**" $ do
        route   $ gsubRoute "static/" (\_ -> "")
        compile copyFileCompiler

      match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

      match "scss/**" $ do
        route   $ gsubRoute "scss" (\_ -> "css")
        compile $ sassCompilerWith def{ sassIncludePaths = Just ["scss"] }

      match "js/**" $ do
        route   idRoute
        compile compressJsCompiler

      match "copy/entries/*" $ do
          route   routeEntry
          compile $ do
            e <- saveSnapshot "entry" =<< compileEntry
            makeItem ("" :: String)

      match "copy/entries/*" . version "markdown" $ do
        route   $ routeEntry `composeRoutes` setExtension "md"
        compile entryMarkdownCompiler

      match "copy/entries/*" . version "latex" $ do
        route   $ routeEntry `composeRoutes` setExtension "tex"
        compile $ entryLaTeXCompiler lTempl

      homePag <- buildPaginateWith
                   mkHomePages
                   "copy/entries/*"
                   (\i -> fromFilePath ("home" </> show i))
      paginateRules homePag $ \i p -> do
        route idRoute
        compile $ do
          contents <- map (fmap (T.unpack . entryLede))
                        <$> loadAllSnapshots p "entry"
          renders <- traverse (renderPandocWith entryReaderOpts entryWriterOpts)
                       contents
          makeItem $ unlines (map itemBody renders)

      create ["index.html"] $ do
        route idRoute
        compile $ do
          home1 <- itemBody <$> load "home/1"
          makeItem (home1 :: String)

    -- create ["archive.html"] $ do
    --     route idRoute

        -- compile $ blazeCompiler (viewHome undefined)

  where
    routeEntry :: Routes
    routeEntry = metadataRoute $ \m ->
        fromMaybe (setExtension ""
                     `composeRoutes`
                     gsubRoute "copy/entries/" (\_ -> "entry/ident/")
                  )
          $ asum [ constRoute . ("entry" </>)       <$> M.lookup "slug" m
                 , constRoute . ("entry/ident" </>) <$> M.lookup "identifier" m
                 -- , constRoute . ("entry/id" </>)    <$> M.lookup "entry-id" m
                 ]
    mkHomePages :: MonadMetadata m => [Identifier] -> m [[Identifier]]
    mkHomePages ids = do
      withDates <- fmap catMaybes
                 . forM ids $ \i -> runMaybeT $ do
        dString <- MaybeT $ getMetadataField i "date"
        d <- maybe mzero return $ parseETime dString
        return (d :: LocalTime, i)
      let sorted = map snd
                 . sortBy (flip $ comparing fst)
                 $ withDates
      return $ paginateEvery 4 sorted


compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap f <$> getResourceString
  where
    f :: String -> String
    f = TL.unpack . TL.decodeUtf8 . minify . TL.encodeUtf8 . TL.pack


-- entryTexFull :: Maybe String -> Entry -> T.Text
-- entryTexFull temp e = case temp of
--     Just t  -> let opts' = opts { P.writerStandalone = True
--                                 , P.writerTemplate   = t
--                                 }
--                in  T.pack . P.writeLaTeX opts' $ pd
--     Nothing -> T.pack . P.writeLaTeX opts $ pd
--   where
--     pd      = P.walk upgrade rawPd
--     rawPd   = entryPandoc (e { entryContent = mdHeader })
--     mdHeader = T.unlines [ T.append "% " eTitle
--                          , T.append "% " . authorInfoName . siteDataAuthorInfo $ siteData
--                          , eDate
--                          , T.empty
--                          , T.concat [ "*Originally posted on **"
--                                     , "["
--                                     , siteDataTitle siteData
--                                     , "]("
--                                     , renderUrl' "/"
--                                     , ")**.*"
--                                     ]
--                          , T.empty
--                          , entryContent e
--                          ]
--     eDate   = case entryPostedAt e of
--                 Just t  -> T.append "% " . T.pack . renderShortFriendlyTime $ t
--                 Nothing -> T.empty
--     eTitle  = T.unwords . T.lines $ entryTitle e
--     -- opts = pandocWriterOptions { P.writerNumberSections = True }
--     opts = pandocWriterOptions
--     upgrade (P.Header n t xs) | n > 1 = P.Header (n-1) t xs
--     upgrade (P.Div di@(_,is,_) xs) | "note" `elem` is = P.Div di (P.HorizontalRule : xs ++ [P.HorizontalRule])
--     upgrade x               = x

-- data Entry = Entry
--     { entryTitle      :: T.Text
--     , entryContent    :: T.Text
--     , entryPandoc     :: Pandoc
--     , entrySourceFile :: Maybe FilePath
--     , entryCreateTime :: Maybe LocalTime
--     , entryPostTime   :: Maybe LocalTime
--     , entryModifyTime :: Maybe LocalTime
--     , entryIdentifier :: Maybe T.Text
--     , entrySlug       :: Maybe T.Text
--     , entryOldSlugs   :: [T.Text]
--     }
--   deriving (Show)


-- pandocReaderOptions = (P.def P.ReaderOptions)

-- pandocReaderOptions :: P.ReaderOptions
-- pandocReaderOptions = (P.def P.ReaderOptions)
--                       { P.readerSmart = True
--                       }

-- dropEnd :: Int -> [a] -> [a]
-- dropEnd i xs = zipWith const xs (drop i xs)