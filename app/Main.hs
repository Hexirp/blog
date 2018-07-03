{-# LANGUAGE OverloadedStrings #-}

module Main where
 import Prelude

 import System.FilePath ((</>))

 import Hakyll

 main :: IO ()
 main = hakyllWith config $ do
  iconRule
  templatesRule
  stylesRule
  articlesRule

 config :: Configuration
 config = defaultConfiguration {
  providerDirectory = "docs-pre",
  destinationDirectory = "docs"}

 iconRule :: Rules ()
 iconRule = match "icon.png" $ do
  route idRoute
  compile copyFileCompiler

 templatesRule :: Rules ()
 templatesRule = match "templates" </> "*" $ compile templateCompiler

 stylesRule :: Rules ()
 stylesRule = match "styles" </> "*" $ do
  route idRoute
  compile $ compressCssCompiler

 articlesRule :: Rules ()
 articlesRule = match "articles" </> "*" $ do
  route $ setExtension "html"
  compile pandocCompiler
