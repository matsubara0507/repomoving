module Repomoving.Cmd where

import           RIO
import qualified RIO.Text               as T

import           Data.Fallible
import qualified Git.Cmd                as Git
import qualified GitHub
import qualified GitHub.Endpoints.Repos as GitHub
import qualified Mix.Plugin.GitHub      as MixGitHub
import qualified Mix.Plugin.Logger      as MixLogger
import qualified Mix.Plugin.Shell       as MixShell
import           Repomoving.Config      (appendAffix)
import           Repomoving.Env

cmd :: RIO Env ()
cmd = do
  repositories <- asks (view #repositories . view #config)
  for_ repositories $ \repo -> evalContT $ do
    _ <- lift (createRepository repo) !?? warn repo "cannot create repository"
    _ <- lift (copyRepository repo)
    MixLogger.logInfo $ display ("success: " <> repo)
  where
    warn r msg = exit $ MixLogger.logWarn (display $ T.pack msg <> ": " <> r)

deleteRepositories :: RIO Env ()
deleteRepositories = do
  repositories <- asks (view #repositories . view #config)
  for_ repositories $ \repo -> deleteRepository repo >>= \case
    Nothing -> MixLogger.logError $ display ("cannot delete repo: " <> repo)
    Just _  -> MixLogger.logInfo  $ display ("success: " <> repo)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

createRepository :: Text -> RIO Env (Maybe ())
createRepository name = do
  config <- asks (view #config)
  let owner' = GitHub.mkName Proxy $ config ^. #to
      name'  = GitHub.mkName Proxy $ name `appendAffix` config
  resp <- MixGitHub.fetch $ GitHub.createOrganizationRepoR owner' (GitHub.newRepo name')
  case resp of
    Left _  -> pure Nothing
    Right _ -> pure (Just ())

deleteRepository :: Text -> RIO Env (Maybe ())
deleteRepository name =  do
  config <- asks (view #config)
  let (owner', name') = (GitHub.mkName Proxy $ config ^. #from, GitHub.mkName Proxy name)
  resp <- MixGitHub.fetch $ GitHub.deleteRepoR owner' name'
  case resp of
    Left _  -> pure Nothing
    Right _ -> pure (Just ())

copyRepository :: Text -> RIO Env ()
copyRepository repo = do
  config <- asks (view #config)
  (fromUrl, toUrl) <- buildRepositoryUrls repo
  MixShell.exec $ Git.clone [fromUrl, repo]
  local (over #work $ toWorkWith repo') $ MixShell.exec $ do
    _ <- Git.pullAllUntrackingBranches "origin"
    branches <- Git.branches []
    Git.remote ["add", config ^. #to, toUrl]
    for_ branches $ \branch -> do
      Git.checkout [branch]
      Git.push ["-u", config ^. #to, branch]
  where
    repo' = T.unpack repo

buildRepositoryUrls :: Text -> RIO Env (Text, Text)
buildRepositoryUrls repo = do
  config <- asks (view #config)
  token  <- MixGitHub.tokenText
  let fromUrl = mconcat ["https://", token, "@github.com/", config ^. #from, "/", repo , ".git"]
      toUrl   = mconcat ["https://", token, "@github.com/", config ^. #to, "/", repo `appendAffix` config, ".git"]
  pure (fromUrl, toUrl)

toWorkWith :: FilePath -> FilePath -> FilePath
toWorkWith path = (<> "/" <> path)
