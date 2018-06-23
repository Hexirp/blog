module Main where
 import Prelude

 import Hakyll

 main :: IO ()
 main = hakyllWith config $ return ()

 config :: Configuration
 config = defaultConfiguration {
  providerDirectory = "docs-pre",
  destinationDirectory = "docs"}
