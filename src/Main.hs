module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Csv
import           Development.Shake
import           FSL
import           Preproc
import Development.Shake.Config

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    usingConfigFile "hcp.cfg"
    want ["build/topup/topup_Pos_Neg_b0.nii.gz"]

    "build/topup/topup_Pos_Neg_b0.nii.gz" %> \out -> do
      let deps@[b0s, acqparams, topupcfg] =
            ["build/topup/B0s.nii.gz"
            ,"build/topup/acqparams.txt"
            ,"b02b0.cnf"]
      need deps
      command [] "topup" ["--imain="++b0s
                         ,"--datain="++acqparams
                         ,"--config="++topupcfg
                         ,"--out="++out
                         ,"-v"]

    [ "build/topup/PosNeg.nii.gz",
      "build/topup/Pos_B0.nii.gz",
      "build/topup/Neg_B0.nii.gz",
      "build/topup/B0s.nii.gz",
      "build/topup/acqparams.txt",
      "build/topup/index.txt",
      "build/topup/preproc-summary.csv"] *>>
      \[outvol, pos_b0, neg_b0, b0s, acqparams, outindex, summaryCsv] -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        Just phasedir <- fmap read <$> getConfig "phase"
        Just echospacing <- fmap read <$> getConfig "echospacing"
        need $ posdwis ++ negdwis
        let
          readDWIPair (pid, dwi, dwi') =
            mkDWIPair <$>
              pure pid <*>
              pure dwi <*>
              pure dwi' <*>
              readbval (tobval dwi) <*>
              readbval (tobval dwi')
          toRecords (DWIPair i i') = [i,i']
        ps <- traverse readDWIPair $ zip3 [1..] posdwis negdwis
        writeFile' summaryCsv $ B.unpack $ encodeDefaultOrderedByName (concatMap toRecords ps)
        writeB0s pos_b0 (map pos ps)
        writeB0s neg_b0 (map neg ps)
        mergeVols b0s [pos_b0, neg_b0]
        writeCombined outvol ps
        writeIndex outindex ps
        writeAcqparams acqparams phasedir echospacing ps
