{-# LANGUAGE OverloadedStrings #-}

-- import Network.Wai
-- import Web.Blog.Database
import Control.Monad.IO.Class
import Development.Blog.Util
import Network.Wai.Middleware.Headers
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.Routes
import Web.Scotty
import Network.Wai.Handler.CGI

main :: IO ()
main = do
  a <- scottyApp app
  run a

app :: ScottyM ()
app = do

  liftIO startupHelpers

  middleware logStdoutDev
  middleware $ addHeaders [("Cache-Control","max-age=86400")]
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ staticPolicy (noDots >-> addBase "tmp/static")
  middleware $ addHeaders [("Cache-Control","max-age=0")]

  route
