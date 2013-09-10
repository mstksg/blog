
module Development.Blog.Util.BuildEntryPage (buildEntryPages) where

-- import Web.Blog.Render
-- import Web.Blog.Types
-- import qualified Data.Foldable                as Fo
import Control.Monad.Reader
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Routes.Entry
import Web.Blog.Views.Layout
import qualified Data.Text                       as T
import qualified Database.Persist.Postgresql     as D
import qualified Text.Blaze.Html.Renderer.String as B
import qualified Web.Scotty                      as S

buildEntryPages :: S.ActionM ()
buildEntryPages = do
  es <- liftIO $ runDB
    (D.selectList [] [] :: D.SqlPersistM [D.Entity Entry])
  mapM_ buildEntryPage es

buildEntryPage :: D.Entity Entry-> S.ActionM ()
buildEntryPage eEntity@(D.Entity eKey _) = do
  urlPath <- liftIO $ runDB $ getUrlPath eEntity

  let
    staticPath = T.append "/static" urlPath
    Right eIdText = D.fromPersistValueText $ D.unKey eKey

  slug <- liftIO $ runDB $ getCurrentSlug eEntity
  Right (v,d) <- case slug of
    Just (D.Entity _ s) -> routeEntrySlug $ slugSlug s
    Nothing             -> routeEntryId $ read $ T.unpack eIdText

  ran <- runReaderT (viewLayout v) d

  liftIO $ writeFile (T.unpack staticPath) (B.renderHtml ran)
