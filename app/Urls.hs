{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Urls
  ( listR
  , listU
  , listP
  , dirR
  , dirU
  , dirP
  ) where

import Security

import Data.Text (Text)
import System.FilePath ((</>),(<.>))
import Web.Scotty (ActionM,RoutePattern)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.HTTP.Types as Http
import qualified Web.Scotty as Scotty

------------ Lists ------------

listR :: RoutePattern
listR = Scotty.regex "^/~([a-z-][a-z0-9-]*)/(.+)\\.ld$"

listU :: User auth -> FilePath -> Text
listU user filepath = T.pack $ "/~" <> userPath user </> filepath <.> "ld"

listP :: ActionM (User 'NoAuth, FilePath)
listP = do
  username <- Scotty.param "1"
  listpath <- Scotty.param "2"
  user <- case username :: LT.Text of
    "marbou" -> pure Me
    _ -> Scotty.raiseStatus Http.status403 $ LT.concat
      [ "you cannot view this user's lists: "
      , username
      ]
  pure (user, LT.unpack listpath)

------------ Directories ------------

-- WARNING must match after 'listR'
dirR :: RoutePattern
dirR = Scotty.regex "^/~([a-z-][a-z0-9-]*)/(.*)$"

dirU :: User owner -> FilePath -> Text
dirU owner urlpath = T.pack $ "/~" <> userPath owner </> urlpath

dirP :: ActionM (User 'NoAuth, FilePath)
dirP = do
  username <- Scotty.param "1"
  listpath <- Scotty.param "2"
  user <- case username :: LT.Text of
    "marbou" -> pure Me
    _ -> Scotty.raiseStatus Http.status403 $ LT.concat
      [ "you cannot view this user's lists: "
      , username
      ]
  pure (user, LT.unpack listpath)
