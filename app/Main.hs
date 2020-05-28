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
import           Mix.Plugin.Logger      as MixLogger
import           Repomoving.Cmd
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help       -> hPutBuilder stdout (fromString usage)
     | r ^. #version    -> hPutBuilder stdout (Version.build version <> "\n")
     | not (validate r) -> hPutBuilder stderr "invalid arguments"
     | otherwise        -> runCmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #from    @= fromOpt
        <: #to      @= toOpt
        <: #prefix  @= prefixOpt
        <: #suffix  @= suffixOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "from"    >: Text
   , "to"      >: Text
   , "prefix"  >: Maybe Text
   , "suffix"  >: Maybe Text
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

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  repos <- Y.decodeFileThrow $ fromMaybe "./config.yaml" path :: IO [Text]
  let config = shrink $ #repositories @= repos <: opts
      plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> MixConfig.buildPlugin config
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil
