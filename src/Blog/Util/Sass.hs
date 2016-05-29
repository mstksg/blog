{-# LANGUAGE ImplicitParams #-}

module Blog.Util.Sass where

import Blog.Types
import Blog.View
import Text.Sass
import Data.Reflection
import Data.Tagged

renderSassUrl
    :: Reifies s Config
    => Tagged s SassFunction
renderSassUrl = Tagged . SassFunction "render-url($x)" $ \v -> return $
    case v of
      SassList [SassString l] _ -> SassString $ "url(\"" ++ renderRootUrl' l ++ "\")"
      _            -> v
