module Git.Cmd where

import           RIO
import qualified RIO.Text as T

import           Shelly   hiding (FilePath, unlessM)

clone :: [Text] -> Sh ()
clone = command1_ "git" [] "clone"

fetch :: [Text] -> Sh ()
fetch = command1_ "git" [] "fetch"

pull :: [Text] -> Sh ()
pull = command1_ "git" [] "pull"

remote :: [Text] -> Sh ()
remote = command1_ "git" [] "remote"

push :: [Text] -> Sh ()
push = command1_ "git" [] "push"

checkout :: [Text] -> Sh ()
checkout = command1_ "git" [] "checkout"

commit :: [Text] -> Sh ()
commit = command1_ "git" [] "commit"

add :: [Text] -> Sh ()
add = command1_ "git" [] "add"

branch :: [Text] -> Sh ()
branch = command1_ "git" [] "branch"

cloneOrFetch :: Text -> Text -> Sh ()
cloneOrFetch repoUrl repoName = do
  dir <- pwd
  let repoDir = dir </> repoName
  unlessM (test_d repoDir) $ clone [repoUrl, repoName]
  chdir repoDir $ fetch ["origin"]

existBranch :: Text -> Sh Bool
existBranch branchName = do
  bs <- T.lines <$> command1 "git" [] "branch" []
  pure $ any (T.isSuffixOf branchName) bs

pullAllUntrackingBranches :: Text -> Sh [Text]
pullAllUntrackingBranches remoteName = do
  fetch ["--all"]
  localBranches  <- branches []
  remoteBranches <- branches ["--remote"]
  let bs = filter (validate $ "HEAD" : localBranches) remoteBranches
  for_ bs $ \b -> checkout ["--track", b]
  pure bs
  where
    validate :: [Text] -> Text -> Bool
    validate localBranches br = and
      [ T.isPrefixOf (remoteName <> "/") br
      , not $ any (`T.isSuffixOf` br) localBranches
      ]

branches :: [Text] -> Sh [Text]
branches opts =
  T.lines <$> command1 "git" [] "branch"  ("--format=%(refname:short)" : opts)
