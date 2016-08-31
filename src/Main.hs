module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Csv
import           Development.Shake
import           FSL
import           Preproc

pas :: [FilePath]
pas = [
  "BIO_0001.dwiPA1.nii.gz",
  "BIO_0001.dwiPA2.nii.gz"
  ]

aps :: [FilePath]
aps = [
  "BIO_0001.dwiAP1.nii.gz",
  "BIO_0001.dwiAP2.nii.gz"
  ]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    want ["build/topup/PosNeg.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    [ "build/topup/PosNeg.nii.gz",
      "build/topup/B0s.nii.gz",
      "build/topup/acqparams.txt",
      "build/topup/index.txt",
      "build/topup/summary.csv"] *>>
      \[outvol, outb0s, acqparams, outindex, summaryCsv] -> do
      need $ pas ++ aps
      let
        readDWIPair (pid, dwi, dwi') =
          mkDWIPair <$>
            pure pid <*>
            pure dwi <*>
            pure dwi' <*>
            readbval (tobval dwi) <*>
            readbval (tobval dwi')
        toRecords (DWIPair a b') = [a,b']
      dwiPairs <- traverse readDWIPair $ zip3 [1..] pas aps
      writeFile' summaryCsv $ B.unpack $ encodeDefaultOrderedByName (concatMap toRecords dwiPairs)
      writeB0s outb0s $ (map pos dwiPairs) ++ map neg dwiPairs
      writeCombined outvol dwiPairs
      writeIndex outindex dwiPairs
      writeAcqparams acqparams dwiPairs
