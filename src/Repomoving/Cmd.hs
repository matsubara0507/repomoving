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
    _ <- lift (changeDefaultBranch repo) !?? warn repo "cannnot change default branch"
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
      repo   = (GitHub.newRepo name') { GitHub.newRepoPrivate = Just (config ^. #private) }
  resp <- MixGitHub.fetch $ GitHub.createOrganizationRepoR owner' repo
  case resp of
    Left _  -> pure Nothing
    Right _ -> pure (Just ())

deleteRepository :: Text -> RIO Env (Maybe ())
deleteRepository name = do
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

changeDefaultBranch :: Text -> RIO Env (Maybe ())
changeDefaultBranch name = do
  config <- asks (view #config)
  branch <- fetchDefaultBranch (config ^. #from) name
  flip (maybe $ pure Nothing) branch $
    updateDefaultBranch (config ^. #to) (name `appendAffix` config)

fetchDefaultBranch :: Text -> Text -> RIO Env (Maybe Text)
fetchDefaultBranch owner name = do
  resp <- MixGitHub.fetch $ GitHub.repositoryR owner' name'
  case resp of
    Right r -> pure $ GitHub.repoDefaultBranch r
    Left _  -> pure Nothing
  where
    owner' = GitHub.mkName Proxy owner
    name'  = GitHub.mkName Proxy name

updateDefaultBranch :: Text -> Text -> Text -> RIO Env (Maybe ())
updateDefaultBranch owner name branch = do
  config <- asks (view #config)
  let repo = defEditRepo
        { GitHub.editDefaultBranch = Just branch
        , GitHub.editPrivate = Just (config ^. #private)
        }
  resp <- MixGitHub.fetch $ GitHub.editRepoR owner' name' repo
  case resp of
    Right _ -> pure $ Just ()
    Left _  -> pure Nothing
  where
    owner' = GitHub.mkName Proxy owner
    name'  = GitHub.mkName Proxy name

defEditRepo :: GitHub.EditRepo
defEditRepo =
  GitHub.EditRepo
    Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing
