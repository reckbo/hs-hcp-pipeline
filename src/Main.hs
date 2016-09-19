module Main where

import           Development.Shake
import           Development.Shake.Config
import qualified Stage.Eddy               as Eddy
import qualified Stage.Normalize          as Normalize
import qualified Stage.PostEddy           as PostEddy
import qualified Stage.Preprocessing      as Preprocessing
import qualified Stage.Topup              as Topup

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
