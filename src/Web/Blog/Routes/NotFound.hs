{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.NotFound (routeNotFound) where

-- import Control.Applicative      ((<$>))
-- import Data.List                (find)
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.NotFound
import Control.Monad.Trans.Error
import qualified Data.Text.Lazy as L

routeNotFound :: Maybe L.Text -> RouteEither
routeNotFound err = do

  let
    view = viewNotFound
    pageData' = pageData { pageDataTitle = Just "Not Found" 
                         , pageDataCss   = ["/css/page/not-found.css"] }

  case err of
    Just _  -> ErrorT $ return $ Left "/not-found"
    Nothing -> ErrorT $ return $ Right (view, pageData')

