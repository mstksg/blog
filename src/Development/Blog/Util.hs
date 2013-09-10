module Development.Blog.Util (
    startupHelpers
  , startupHelpers'
  , backupEntries
  ) where

import Development.Blog.Util.Compass
import Development.Blog.Util.LoadEntries
import Development.Blog.Util.BuildEntryPage
import Control.Monad.IO.Class
import Development.Blog.Util.BackupEntries
import Web.Blog.Database
import qualified Web.Scotty as S

entriesDir :: FilePath
entriesDir = "copy/entries"

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    runDB blogMigrate
    loadEntries entriesDir

startupHelpers' :: S.ActionM ()
startupHelpers' = do
    liftIO startupHelpers
    buildEntryPages
