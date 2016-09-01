module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Csv (encodeDefaultOrderedByName)
import           Development.Shake
import           Development.Shake.Config
import           FSL
import           Preproc
import           Text.Printf

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]


    usingConfigFile "hcp.cfg"
    want ["build/eddy/eddy_unwarped_images.nii.gz"]


    -- Eddy

    "build/eddy/eddy_unwarped_images.nii.gz" %> \out -> do
      let deps@[vol,mask,index,acqp,bvec,bval,topupb0] =
               ["build/topup/PosNeg.nii.gz"
               ,"build/topup/nodif_brain_mask.nii.gz"
               ,"build/topup/index.txt"
               ,"build/topup/acqparams.txt"
               ,"build/preproc/PosNeg.bvec"
               ,"build/preproc/PosNeg.bval"
               ,"build/topup/topup_Pos_Neg_b0.nii.gz"]
      need deps
      unit $ command [] "eddy" ["--imain="++vol
                               ,"--mask="++mask
                               ,"--index="++index
                               ,"--acqp="++acqp
                               ,"--bvecs="++bvec
                               ,"--bval="++bval
                               ,"--fwhm=0"
                               ,"--topup="++topupb0
                               ,"--flm=quadratic"
                               ,"-v"
                               ,"--out="++out]

    -- Topup

    ["build/topup/nodif_brain.nii.gz",
     "build/topup/nodif_brain_mask.ni.gz"] *>> \[out,_] -> do
      let hifib0 = "build/topup/hifib0.nii.gz"
      need [hifib0]
      command [] "bet" [hifib0, out, "-m", "-f", "0.2"]

    "build/topup/hifib0.nii.gz" %> \out -> do
      let deps@[posb0,negb0,_,_,params] =
            ["build/topup/Pos_B0.nii.gz"
            ,"build/topup/Neg_B0.nii.gz"
            ,"build/topup/topup_Pos_Neg_b0_fieldcoef.nii.gz"
            ,"build/topup/topup_Pos_Neg_b0_movpar.txt"
            ,"build/topup/acqparams.txt"]
      need deps
      dimt <- (+1) <$> getDim4 posb0
      posb01 <- extractVol posb0 1
      negb01 <- extractVol negb0 1
      unit $ command [] "applytopup"
        [printf "--imain=%s,%s" posb01 negb01
        ,"--topup=build/topup/topup_Pos_Neg_b0"
        ,"--datain="++params
        ,"--inindex=1,"++ show dimt
        ,"--out="++out]
      liftIO $ removeFiles "." [posb01,negb01]

    ["topup_Pos_Neg_b0_fieldcoef.nii.gz",
     "topup_Pos_Neg_b0_movpar.txt"]  *>> \_ -> do
      let deps@[b0s, acqparams, topupcfg] =
            ["build/topup/B0s.nii.gz"
            ,"build/topup/acqparams.txt"
            ,"b02b0.cnf"]
      need deps
      command [] "topup" ["--imain="++b0s
                         ,"--datain="++acqparams
                         ,"--config="++topupcfg
                         ,"--out=build/topup/topup_Pos_Neg_b0"
                         ,"-v"]

    -- Preprocessing

    "build/preproc/PosNeg.bval" %> \out -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        posbvals <- concat <$> traverse (readbval . tobval) posdwis
        negbvals <- concat <$> traverse (readbval . tobval) negdwis
        writebval out $ posbvals ++ negbvals

    "build/preproc/PosNeg.bvec" %> \out -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        posbvecs <- concat <$> traverse (readbvec . tobvec) posdwis
        negbvecs <- concat <$> traverse (readbvec . tobvec) negdwis
        writebvec out $ posbvecs ++ negbvecs

    ["build/topup/PosNeg.nii.gz",
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
