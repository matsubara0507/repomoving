module Repomoving.Cmd where

import           RIO

import           Repomoving.Env

cmd :: RIO Env ()
cmd = showNotImpl

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
