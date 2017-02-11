{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hakyll.Web.Blaze where

import           Blog.Render
import           Blog.Types
import           Blog.View
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe
import           Hakyll
import           System.FilePath
import qualified Data.Text                       as T
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5                as H

blazeCompiler
    :: (?config :: Config)
    => PageData
    -> H.Html
    -> Compiler (Item String)
blazeCompiler pd body = do
    r <- getRoute =<< getUnderlying
    ps <- map itemBody <$>
            loadAllSnapshots
                ("copy/pages/*" .&&. hasVersion "page")
                "page"

    let r'  = (<|> r) $ do
          rC <- r
          st <- T.stripSuffix "/index.html" $ T.pack rC
          return $ T.unpack st
        pd' = case pageDataCanonical pd of
                Nothing -> pd { pageDataCanonical = r' }
                Just _  -> pd
        ps' = flip mapMaybe ps $ \Page{..} -> do
                l <- pageLink
                u <- pageUrl
                return (l, T.pack $ T.unpack u <.> "html")
        h   = renderLayout ps' pd' body
    makeItem $ H.renderHtml h
