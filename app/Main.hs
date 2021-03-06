module Main where

import           Paths_repomoving       (version)
import           RIO
import qualified RIO.Text               as T

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified Data.Yaml              as Y
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Config      as MixConfig
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           Repomoving.Cmd         as Cmd
import           Repomoving.Env
import           System.Environment     (getEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help       -> hPutBuilder stdout (fromString usage)
     | r ^. #version    -> hPutBuilder stdout (Version.build version <> "\n")
     | not (validate r) -> hPutBuilder stderr "invalid arguments"
     | r ^. #delete     -> runCmd Cmd.deleteRepositories r (listToMaybe args)
     | otherwise        -> runCmd cmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #from    @= fromOpt
        <: #to      @= toOpt
        <: #prefix  @= prefixOpt
        <: #suffix  @= suffixOpt
        <: #work    @= workOpt
        <: #private @= privateOpt
        <: #delete  @= deleteOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "from"    >: Text
   , "to"      >: Text
   , "prefix"  >: Maybe Text
   , "suffix"  >: Maybe Text
   , "work"    >: FilePath
   , "private" >: Bool
   , "delete"  >: Bool
   ]

validate :: Options -> Bool
validate opts = not $ T.null (opts ^. #from) || T.null (opts ^. #to)

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

fromOpt :: OptDescr' Text
fromOpt = maybe "" fromString <$> optLastArg [] ["from"] "OWNER" "Move repositories from OWNER"

toOpt :: OptDescr' Text
toOpt = maybe "" fromString <$> optLastArg [] ["to"] "OWNER" "Move repositories to OWNER"

prefixOpt :: OptDescr' (Maybe Text)
prefixOpt = fmap fromString <$> optLastArg [] ["prefix"] "TEXT" "Prefix for moved repositories name"

suffixOpt :: OptDescr' (Maybe Text)
suffixOpt = fmap fromString <$> optLastArg [] ["suffix"] "TEXT" "Suffix for moved repositories name"

workOpt :: OptDescr' FilePath
workOpt = fromMaybe ".repomoving" <$> optLastArg [] ["work"] "PATH" "Work direcotry path"

privateOpt :: OptDescr' Bool
privateOpt = optFlag [] ["private"] "Make private repositories"

deleteOpt :: OptDescr' Bool
deleteOpt = optFlag [] ["delete"] "Delete target reposirtories"

runCmd :: (RIO Env ()) -> Options -> Maybe FilePath -> IO ()
runCmd act opts path = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  repos  <- Y.decodeFileThrow $ fromMaybe "./config.yaml" path :: IO [Text]
  let config = shrink $ #repositories @= repos <: opts
      plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #github <@=> MixGitHub.buildPlugin gToken
            <: #config <@=> MixConfig.buildPlugin config
            <: #work   <@=> pure (opts ^. #work)
            <: nil
  Mix.run plugin act
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
