module Main where
 import Prelude

 import Hakyll

main :: IO ()
main = return ()

hakyllConfig :: Configuration
hakyllConfig = def {
 providerDirectory = "site",
 destinationDirectory = "docs"}
