module Main where
 import Prelude

 import Hakyll

 main :: IO ()
 main = return ()

 hakyllConfig :: Configuration
 hakyllConfig = def {
  providerDirectory = "docs-pre",
  destinationDirectory = "docs"}
