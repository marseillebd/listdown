{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.List
  ( listHtml
  ) where

import Lucid.Html5

import Control.Monad (forM_)
import Listdown.Text (toTextFile)
import Listdown.Types (File)
import Lucid (Html,toHtml)
import Security (Permit,PermitLevel(..),PermitAct(..))
import Urls (dirU)

listHtml :: Maybe (Permit 'Read 'ForDir) -> File -> Html ()
listHtml parent_m file = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "utf-8" ]
    title_ "Listdown"
    link_ [ rel_ "stylesheet", href_ "/static/main.css" ]
    script_ [type_ "text/javascript", src_ "/static/autosave.js"] (pure () :: Html ())
  body_ $ do
    div_ $ do
      forM_ parent_m $ \parent -> do
        a_ [href_ $ dirU parent.owner parent.urlpath <> "/"] $ do
          toHtml $ dirU parent.owner parent.urlpath
    div_ [class_ "listdown"] $ do
      form_ [ method_ "POST", id_ "full-list-edit", autocomplete_ "off" ] $ do
        div_ [class_ "save-state", data_ "state" "saved", data_ "for" "full-list-edit"] $ do
          span_ [class_ "saved"] $ "✓"
          span_ [class_ "unsaved"] $ do
            "⚠"
            button_ [ type_ "submit" ] $ do
              "Save"
          span_ [class_ "saving"] $ "↻" -- TODO a cancel button
          span_ [class_ "error"] $ do
            "✗ "
            span_ [class_ "error-message"] $ pure ()
            button_ [ type_ "submit" ] $ do
              "Retry"
        textarea_ [ name_ "body" ] $ do
          toHtml $ toTextFile file
