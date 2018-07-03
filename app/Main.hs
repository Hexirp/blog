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
 templatesRule = matchGlob ("templates" </> "*") $ compile templateCompiler

 stylesRule :: Rules ()
 stylesRule = matchGlob ("styles" </> "*") $ do
  route idRoute
  compile $ compressCssCompiler

 articlesRule :: Rules ()
 articlesRule = matchGlob ("articles" </> "*") $ do
  route $ setExtension "html"
  compile pandocCompiler

 -------------------------------------------------------------------------------

 matchGlob :: String -> Rules () -> Rules ()
 matchGlob path rule = match (fromGlob path) rule
