{-# LANGUAGE OverloadedStrings #-}

module Main where
 import Prelude

 import Data.List (intercalate)

 import GHC.IO.Encoding (setLocaleEncoding, utf8)

 import System.FilePath (pathSeparator, (</>))

 import Text.Pandoc
 import Hexyll
 import Hexyll.Web.Pandoc

 main :: IO ()
 main = do
  setLocaleEncoding utf8
  hakyllWith config $ do
   iconRule
   templatesRule
   stylesRule
   articlesRule
   indexRule

 config :: Configuration
 config = defaultConfiguration {
  providerDirectory = "docs-pre",
  destinationDirectory = "docs"}

 iconRule :: Rules ()
 iconRule = do
  match "icon.png" $ do
   route idRoute
   compile copyFileCompiler
  match "small_icon.png" $ do
   route idRoute
   compile copyFileCompiler

 templatesRule :: Rules ()
 templatesRule = match' ["templates", "*"] $ compile templateCompiler

 stylesRule :: Rules ()
 stylesRule = match' ["styles", "*"] $ do
  route idRoute
  compile $ compressCssCompiler

 articlesRule :: Rules ()
 articlesRule = do
  match' ["articles", "*.rst"] $ do
   route $ setExtension "html"
   compile $ myCompiler
  match' ["articles", "*", "*"] $ do
   route idRoute
   compile $ copyFileCompiler

 indexRule :: Rules ()
 indexRule = match "index.rst" $ do
  route $ setExtension "html"
  compile $ myCompiler

 -------------------------------------------------------------------------------

 -- この部分はHakyll.Web.Pandocのソースコードを参考にした。

 myCompiler :: Compiler (Item String)
 myCompiler = myPandocCompiler >>= loapplyTmp "default.html" defaultContext

 myPandocCompiler :: Compiler (Item String)
 myPandocCompiler = pandocCompilerWith myReaderOptions myWriterOptions

 myReaderOptions :: ReaderOptions
 myReaderOptions =
  defaultHexyllReaderOptions {
   readerExtensions = myPandocExtensions }

 myWriterOptions :: WriterOptions
 myWriterOptions =
  defaultHexyllWriterOptions {
    writerExtensions = myPandocExtensions }

 myPandocExtensions :: Extensions
 myPandocExtensions =
  enableExtension Ext_east_asian_line_breaks $
   enableExtension Ext_smart $
    pandocExtensions

 -- | templatesフォルダからテンプレートを探して適用する
 loapplyTmp :: String -> Context a -> Item a -> Compiler (Item String)
 loapplyTmp x = loadAndApplyTemplate (ufromFilePath $ "templates" </> x)

 -------------------------------------------------------------------------------

 -- | 区切られたGlob記法でマッチする
 --
 -- WindowsとLinuxとの互換性に関して有用である。
 match' :: [String] -> Rules () -> Rules ()
 match' x rule = match (fromGlob $ joinGlob x) rule

 -- | 区切られたGlob記法をつなぎ合わせる
 joinGlob :: [String] -> String
 joinGlob = intercalate (escapeGlob [pathSeparator])

 -- | fromGlobにおいての特殊文字をエスケープする
 escapeGlob :: String -> String
 escapeGlob [] = []
 escapeGlob ('\\' : xs) = '\\' : '\\' : escapeGlob xs
 escapeGlob ('*' : xs) = '\\' : '*' : escapeGlob xs
 escapeGlob (x : xs) = x : escapeGlob xs
