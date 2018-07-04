{-# LANGUAGE OverloadedStrings #-}

module Main where
 import Prelude

 import Data.List (intercalate)

 import GHC.IO.Encoding (setLocaleEncoding, utf8)

 import System.FilePath (pathSeparator)

 import Hakyll

 main :: IO ()
 main = do
  setLocaleEncoding utf8
  hakyllWith config $ do
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
 templatesRule = match "templates\\\\*" $ compile templateCompiler

 stylesRule :: Rules ()
 stylesRule = match "styles\\\\*" $ do
  route idRoute
  compile $ compressCssCompiler

 articlesRule :: Rules ()
 articlesRule = match "articles\\\\*" $ do
  route $ setExtension "html"
  compile pandocCompiler

 -------------------------------------------------------------------------------

 -- | 区切られたGlob記法をマッチする
 match' :: [String] -> Rules () -> Rules ()
 match' x rule = match (fromGlob $ intercalate (escape pathSeparator) x) rule

 -- | Glob記法での特殊文字をエスケープする
 escape :: Char -> String
 escape
