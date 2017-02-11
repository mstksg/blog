{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Page where

import           Blog.Types
import           Blog.Util
import           Blog.View.Page
import           Data.Default
import           Hakyll
import           Hakyll.Web.Blaze
import qualified Data.Text         as T
import qualified Text.Pandoc       as P
import qualified Text.Pandoc.Error as P
import qualified Text.Pandoc.Walk  as P

compilePage
    :: (?config :: Config)
    => Compiler (Item Page)
compilePage = do
    i         <- getUnderlying
    pBody     <- getResourceBody
    pPandoc   <- readPandocWith entryReaderOpts pBody
    let pContents   = T.pack . P.writeMarkdown entryWriterOpts <$> pPandoc
    pTitle    <- T.pack
               . P.writeMarkdown entryWriterOpts
               . P.handleError
               . P.readMarkdown entryReaderOpts
               . T.unpack . T.unwords . T.lines . T.pack
             <$> getMetadataField' i "title"
    pUrl      <- fmap T.pack <$> getMetadataField i "url"
    pLink     <- fmap T.pack <$> getMetadataField i "link"
    makeItem Page { pageTitle    = pTitle
                  , pageContents = itemBody pContents
                  , pageUrl      = pUrl
                  , pageLink     = pLink
                  }

pageCompiler
    :: (?config :: Config)
    => Compiler (Item String)
pageCompiler = do
    i <- setVersion Nothing <$> getUnderlying
    p <- loadSnapshotBody i "page"
    let pd = def { pageDataTitle   = Just $ pageTitle p
                 , pageDataDesc    = Just $ pageTitle p
                 , pageDataCss     = [ "/css/page/entry.css"
                                     , "/css/pygments.css"
                                     ]
                 , pageDataJs      = [ ]
                 }
    blazeCompiler pd (viewPage p)

