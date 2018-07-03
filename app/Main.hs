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

 templatesRule :: Rules ()
 templatesRule = match "templates/*" $ compile templateCompiler

 stylesRule :: Rules ()
 stylesRule = match "styles/*" $ do
  route idRoute
  compile $ compressCssCompiler

 articlesRule :: Rules ()
 articlesRule = match "articles/*" $ do
  route $ setExtension "html"
  compile pandocCompiler
