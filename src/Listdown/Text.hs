{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Listdown.Text
  ( -- * Parsers
    Parser
  , Error(..)
  , runParser
  , fromTextFile
  -- * Renderers
  , toTextFile
  ) where

import Listdown.Types

import Control.Monad (forM)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as T

newtype Parser a = Ld
  { runParser :: Either Error a }
  deriving (Functor,Applicative,Monad)

data Error
  = TODOerror
  deriving (Show)

fromTextFile :: Text -> Parser File
fromTextFile txt = do
  -- TODO get title, config, and text
  ls <- forM (T.lines txt) $ parseLine
  items <- groupNodes ls >>= mkItems
  pure File
    { title = Nothing -- TODO
    , config = Map.empty -- TODO
    , text = "" -- TODO
    , content = items
    }

------------ Parser ------------

mkItems :: [Node] -> Parser [Item]
mkItems [] = pure []
mkItems (node:nodes) = do
  let (descendants, nodes') = getDescendants node nodes
  sublists <- mkItems descendants
  let item = Item
        { identifier = node.itemStart.identifier
        , status = node.itemStart.status
        , lbl = node.itemStart.lbl
        , text = [Plain node.content] -- TODO inline parsing
        , sublists
        }
  (item:) <$> mkItems nodes'

getDescendants :: Node -> [Node] -> ([Node], [Node])
getDescendants ancestor rest =
  span (\node -> node.indent > ancestor.indent) rest

data Node = Node
  { indent :: Int
  , itemStart :: ItemStart
  , content :: Text
  }

groupNodes :: [Line] -> Parser [Node]
groupNodes = go Nothing
  where
  go mNode [] = pure $ maybe [] (:[]) mNode
  go mNode (l:ls)
    | T.null l.content = go mNode ls -- TODO I may allow paragraphs
    | Just start <- l.itemStart = do
      let node' = Node
            { indent = l.indent
            , itemStart = start
            , content = l.content
            }
      let k = maybe id (:) mNode
      k <$> go (Just node') ls
    | Just node <- mNode
    , l.indent > node.indent = do
      -- TODO I may try to put extra indentation back into the content
      let node' = node{content = node.content <> "\n" <> l.content} :: Node
      go (Just node') ls
    -- TODO if there's a non-line start with non-greater indent
    | otherwise = do -- a non-lineStart with less than expected indent (when no node is active, effectively expect infinite indent)
      let k = maybe id (:) mNode
      k <$> go Nothing ls

---- Per-Line Parsing ----

data Line = Line
  { indent :: Int
  , itemStart :: Maybe ItemStart
  , content :: Text
  }
data ItemStart = ItemStart
  { identifier :: Maybe Identifier
  , status :: Maybe Status
  , lbl :: Maybe Label
  }

parseLine :: Text -> Parser Line
parseLine t = do
  -- every two spaces (TODO or one tab) is an indent
  let ((`div` 2) . T.length -> indent, T.strip -> t') = T.span (==' ') t
  (itemStart, t'') <- parseItemStart t'
  let content = t'' -- TODO
  pure Line{indent,itemStart,content}

parseItemStart :: Text -> Parser (Maybe ItemStart, Text)
parseItemStart t
  | "-" `T.isPrefixOf` t = do
    let t' = (T.stripStart . T.drop 1) t
    let start = ItemStart
          { identifier = Nothing -- TODO
          , status = Nothing -- TODO
          , lbl = Nothing -- TODO
          }
    pure $ (Just start, t')
  | otherwise = pure (Nothing, t)

------------ Render Text ------------

toTextFile :: File -> Text
toTextFile file = mconcat $ toTextItem 0 <$> file.content -- TODO print other file things

toTextItem :: Int -> Item -> Text
toTextItem depth item = mconcat
  [ indent <> "- "
  , flip (maybe "") item.identifier $ \identifier ->
    "&" <> identifier <> " "
  , flip (maybe "") item.status $ \status ->
    "[" <> status <> "] "
  , flip (maybe "") item.lbl $ \label ->
    fromLabel label <> ": "
  , mconcat $ toTextInline (depth + 1) <$> item.text
  , "\n"
  , T.concat (toTextItem (depth + 1) <$> item.sublists)
  ]
  where
  indent = T.replicate depth indent1
  nl = indent <> "\n"
  nextIndent = indent1 <> indent
  nextNl = indent1 <> nl
  indent1 = "  "

toTextInline :: Int -> Inline -> Text
toTextInline depth = \case
  Plain txt -> T.intercalate nl (T.lines txt)
  link@Link{} -> T.concat [ "[", link.name, "](", link.href, ")" ]
  link@Wikilink{} -> T.concat [ "[[", link.name, "]]" ]
  img@Image{} -> T.concat [ "![", img.alttext, "](", img.href, ")" ]
  where
  nl = T.replicate depth "  " <> "\n"
