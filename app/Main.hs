{-# LANGUAGE OverloadedStrings #-}

module Main where
 import Prelude

 import Hakyll

 main :: IO ()
 main = hakyllWith config $ articlesRule

 config :: Configuration
 config = defaultConfiguration {
  providerDirectory = "docs-pre",
  destinationDirectory = "docs"}

 articlesRule :: Rules ()
 articlesRule = match "articles/*" $ do
  route $ setExtension "html"
  compile pandocCompiler
