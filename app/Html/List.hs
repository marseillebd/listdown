{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.List
  ( listHtml
  ) where

import Listdown.Types
import Lucid.Html5

import Control.Monad (when,forM_)
import Listdown.Text (toTextFile)
import Lucid (Html,toHtml)
import Security (Permit,PermitLevel(..),PermitAct(..))
import Urls (dirU)

listHtml :: Maybe (Permit 'Read 'ForDir) -> File -> Html ()
listHtml parent_m file = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "utf-8" ]
    title_ "Listdown"
    meta_ [ name_ "viewport", content_ "width=device-width, initial-scale=1" ]
    link_ [ rel_ "stylesheet", href_ "/static/main.css" ]
    script_ [type_ "text/javascript", src_ "/static/util.js"] (pure () :: Html ())
    script_ [type_ "text/javascript", src_ "/static/autosave.js"] (pure () :: Html ())
    script_ [type_ "text/javascript", src_ "/static/modeView.js"] (pure () :: Html ())
  body_ $ do
    div_ [id_ "header-info"] $ do
      div_ $ do
        forM_ parent_m $ \parent -> do
          a_ [href_ $ dirU parent.owner parent.urlpath] $ do
            toHtml $ dirU parent.owner parent.urlpath
      button_ [ data_ "ld-mode-set" "read"
              , data_ "ld-mode-is" "read"
              , data_ "ld-mode-view-on" "write"
              ] "👁️"
      button_ [ data_ "ld-mode-set" "write"
              , data_ "ld-mode-view-on" "read"
              ] "✏️"
      span_ [class_ "save-state", data_ "state" "saved", data_ "for" "full-list-edit"] $ do
        span_ [class_ "saved"] $ "✓"
        span_ [class_ "unsaved"] $ do
          "⚠"
          button_ [ type_ "submit", form_ "full-list-edit" ] $ do
            "Save"
        span_ [class_ "saving"] $ "↻" -- TODO a cancel button
        span_ [class_ "error"] $ do
          "✗ "
          span_ [class_ "error-message"] $ pure ()
          button_ [ type_ "submit" ] $ do
            "Retry"
    div_ [class_ "listdown"] $ do
      div_ [data_ "ld-mode-view-on" "read"] $ do
        -- TODO title, config, leading text
        listItems file.content
      div_ [data_ "ld-mode-view-on" "write"] $ do
        form_ [ method_ "POST", id_ "full-list-edit", autocomplete_ "off" ] $ do
          textarea_ [ name_ "body" ] $ do
            toHtml $ toTextFile file

listItems :: [Item] -> Html ()
listItems items = ul_ $ forM_ items listItem

listItem :: Item -> Html ()
listItem item = li_ $ do
  -- TODO identifier, status, label
  p_ $ inline item.text
  when (not . null $ item.sublists) $ listItems item.sublists

inline :: [Inline] -> Html ()
inline = mapM_ $ \case
  Plain txt -> toHtml txt
