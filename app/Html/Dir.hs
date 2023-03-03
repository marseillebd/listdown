{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Dir
  ( directoryList
  ) where

import Lucid.Html5
import Security

import Control.Monad (forM_)
import Data.List (inits)
import Lucid (Html,toHtml)
import System.FilePath (joinPath,(</>),splitDirectories,takeBaseName,splitExtension)
import Urls (dirU,listU)

directoryList :: Permit 'Read 'ForDir -> [FilePath] -> Html ()
directoryList permit children = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "utf-8" ]
    title_ "Listdown"
    link_ [ rel_ "stylesheet", href_ "/static/main.css" ]
  body_ $ do
    p_ $ do
      let trails = joinPath <$> inits (splitDirectories permit.urlpath)
      forM_ trails $ \trail -> do
        a_ [href_ $ dirU permit.owner trail <> "/"] $ do
          toHtml $ case trail of
            "" -> "~" <> userPath permit.owner
            _ -> takeBaseName trail
        "/"
    ul_ $ do
      forM_ children $ \child -> case splitExtension child of
        (crumb, ".ld") -> do
          li_ $ do
            "🗎 "
            a_ [href_ $ listU permit.owner (permit.urlpath </> crumb)] $ do
              toHtml crumb
        (crumb, "") ->
          li_ $ do
            a_ [href_ $ dirU permit.owner (permit.urlpath </> crumb)] $ do
              "📁 "
              toHtml crumb
        _ -> pure ()
