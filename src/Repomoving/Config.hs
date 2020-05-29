module Repomoving.Config where

import           RIO

import           Data.Extensible

type Config = Record
  '[ "repositories" >: [Text] -- repository name without owner
   , "from"         >: Text
   , "to"           >: Text
   , "prefix"       >: Maybe Text
   , "suffix"       >: Maybe Text
   , "private"      >: Bool
   ]

appendAffix :: Text -> Config -> Text
appendAffix name config = mconcat
    [ fromMaybe "" $ config ^. #prefix
    , name
    , fromMaybe "" $  config ^. #suffix
    ]
