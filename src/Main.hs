module Main where

import           Development.Shake
import           Development.Shake.Config
import qualified PostEddy
import qualified Eddy
import qualified Topup
import qualified Normalize
import qualified Preprocessing

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do

  usingConfigFile "hcp.cfg"
  want [PostEddy.datavol
       ,PostEddy.nodif]

  phony "clean" $ do
      putNormal "Cleaning files in build"
      removeFilesAfter "hcp-output" ["//*"]

  PostEddy.rules
  Eddy.rules
  Topup.rules
  Preprocessing.rules
  Normalize.rules
