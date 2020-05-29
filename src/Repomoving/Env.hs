module Repomoving.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as MixGitHub
import           Repomoving.Config

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token
   , "config" >: Config
   , "work"   >: FilePath
   ]
