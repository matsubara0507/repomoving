module Repomoving.Env where

import           RIO

import           Data.Extensible
import           Repomoving.Config

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   ]
