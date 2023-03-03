{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Listdown.Types
  ( -- * Major Types
    File(..)
  , Item(..)
  , Inline(..)
  -- * Special Names
  , Identifier
  , Label
  , toLabel
  , fromLabel
  , Status
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Map (Map)

import qualified Data.Text as T

data File = File
  { title :: Maybe Text -- ^ the display name of the file
  , config :: Map Text Text -- ^ TODO what actually needs to be config'd?
  , text :: Text -- ^ descriptive info before the contents
  , content :: [Item] -- ^ top-level list items
  }
  deriving(Show)
data Item = Item
  { identifier :: Maybe Identifier -- ^ the part just after the dash, when it starts with ampersand `- &linkMe`
  , status :: Maybe Status -- ^ the part in brackets just after the dash `- [x]`
  , lbl :: Maybe Label -- ^ the part before the first colon `- [ ] grammar:`
  , text :: [Inline]
  , sublists :: [Item]
  }
  deriving(Show)
data Inline
  = Plain Text
  | Link { name :: Text, href :: Text }
  | Wikilink { name :: Text }
  | Image { alttext :: Text, href :: Text }
  deriving(Show)

type Identifier = Text -- TODO

newtype Label = Label Text
  deriving (Show)
fromLabel :: Label -> Text
fromLabel (Label t) = t
toLabel :: Text -> Maybe Label
toLabel t = if good t then Just (Label t) else Nothing
  where
  good = T.all goodChar
  goodChar c = isAlphaNum c || c == '-'

type Status = Text -- TODO
