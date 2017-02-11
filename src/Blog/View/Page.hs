{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Page where

import           Blog.Types
import           Blog.Util
import           Blog.View
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

viewPage
    :: (?config :: Config)
    => Page
    -> H.Html
viewPage Page{..} = do
    H.div ! A.class_ "entry-section unit span-grid" ! mainSection $ do
      H.article ! A.class_ "tile article" $ do
        H.header $ do

          H.h1 ! A.id "title" $
            H.toHtml $ pageTitle


        H.div ! A.class_ "main-content copy-content" $
          copyToHtml $ T.unpack pageContents
