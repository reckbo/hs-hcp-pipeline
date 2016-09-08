{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Yaml                (decodeFile, encodeFile)
import           Development.Shake
import           Development.Shake.Config
import           FSL
import           Preproc
import           System.FilePath
import           Text.Printf
import Data.List.Split

summaryYaml :: [Char]
summaryYaml = "build/0_normalized/dwipairs.yaml"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    usingConfigFile "hcp.cfg"
    want ["build/3_data/data.nii.gz"
         ,"build/3_data/nodif.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    -- Post Eddy

    ["build/3_data/nodif_brain.nii.gz",
     "build/3_data/nodif_brain_mask.nii.gz",
     "build/3_data/nodif.nii.gz"]
      *>> \[_,_,nodif] -> do
      let dat = "build/3_data/data.nii.gz"
      need [dat]
      -- Remove negative intensity values (caused by spline interpolation) from final data
      command_ [] "fslmaths" [dat, "-thr", "0", dat]
      command_ [] "bet" [dat , "build/3_data/nodif_brain" , "-m" , "-f" , "0.1"]
      command_ [] "fslroi" [dat, nodif, "0", "1"]

    ["build/3_data/data.nii.gz",
     "build/3_data/bvals",
     "build/3_data/bvecs"]
      *>> \[out,_,_] -> do
      let deps@[summaryYaml
               ,posseries
               ,negseries
               ,posbval
               ,posbvec
               ,negbval
               ,negbvec
               ,eddyimages] =
               ["build/0_normalization/dwipairs.yaml"
               ,"build/1_preproc/Pos_SeriesVolNum.txt"
               ,"build/1_preproc/Neg_SeriesVolNum.txt"
               ,"build/1_preproc/Pos.bval"
               ,"build/1_preproc/Pos.bvec"
               ,"build/1_preproc/Neg.bval"
               ,"build/1_preproc/Neg.bvec"
               ,"build/3_eddy/eddy_unwarped_images.nii.gz"
               ]
      need deps
      Just dwipairs <- liftIO $ decodeFile summaryYaml
      let numPos = show $ sum $ map (_size._pos) dwipairs
          numNeg = show $ sum $ map (_size._neg) dwipairs
      withTempFile $ \eddypos ->
        withTempFile $ \eddyneg -> do
          command_ [] "fslroi" [eddyimages, eddypos, "0", numPos]
          command_ [] "fslroi" [eddyimages, eddyneg, numPos, numNeg]
          command_ [] "eddy_combine" [eddypos, posbval, posbvec, posseries
                                     ,eddyneg, negbval, negbvec, negseries
                                     ,takeDirectory out, "1"]


      -- Eddy

    "build/3_eddy/eddy_unwarped_images.nii.gz"
      %> \out -> do
        let deps@[vol,index,acqp,bvec,bval,mask,_,_] =
                ["build/1_preproc/PosNeg.nii.gz"
                ,"build/1_preproc/index.txt"
                ,"build/1_preproc/acqparams.txt"
                ,"build/1_preproc/PosNeg.bvec"
                ,"build/1_preproc/PosNeg.bval"
                ,"build/2_topup/nodif_brain_mask.nii.gz"
                ,"build/2_topup/topup_Pos_Neg_b0_fieldcoef.nii.gz"
                ,"build/2_topup/topup_Pos_Neg_b0_movpar.txt"]
        need deps
        command_ [] "eddy" ["--imain="++vol
                           ,"--mask="++mask
                           ,"--index="++index
                           ,"--acqp="++acqp
                           ,"--bvecs="++bvec
                           ,"--bvals="++bval
                           ,"--fwhm=0"
                           ,"--topup=build/2_topup/topup_Pos_Neg_b0"
                           ,"--flm=quadratic"
                           ,"-v"
                           ,"--out="++out]

      -- Topup

    ["build/2_topup/nodif_brain.nii.gz",
     "build/2_topup/nodif_brain_mask.nii.gz"]
      *>> \[out,_] -> do
      let hifib0 = "build/2_topup/hifib0.nii.gz"
      need [hifib0]
      command [] "bet" [hifib0, out, "-m", "-f", "0.2"]

    "build/2_topup/hifib0.nii.gz"
      %> \out -> do
      let deps@[posb0,negb0,params,_,_] =
            ["build/1_preproc/Pos_B0.nii.gz"
            ,"build/1_preproc/Neg_B0.nii.gz"
            ,"build/1_preproc/acqparams.txt"
            ,"build/2_topup/topup_Pos_Neg_b0_fieldcoef.nii.gz"
            ,"build/2_topup/topup_Pos_Neg_b0_movpar.txt"]
      need deps
      dimt <- (+1) <$> getDim4 posb0
      withTempFile $ \posb01 ->
        withTempFile $ \negb01 -> do
          extractVol_ posb01 posb0 1
          extractVol_ negb01 negb0 1
          command_ [] "applytopup" [printf "--imain=%s,%s" posb01 negb01
                                 ,"--topup=build/2_topup/topup_Pos_Neg_b0"
                                 ,"--datain="++params
                                 ,"--inindex=1,"++ show dimt
                                 ,"--out="++out]

    ["build/2_topup/topup_Pos_Neg_b0_fieldcoef.nii.gz",
     "build/2_topup/topup_Pos_Neg_b0_movpar.txt"]
      *>> \_ -> do
      let deps@[b0s, acqparams, topupcfg] =
            ["build/1_preproc/B0s.nii.gz"
            ,"build/1_preproc/acqparams.txt"
            ,"b02b0.cnf"]
      need deps
      command [] "topup" ["--imain="++b0s
                         ,"--datain="++acqparams
                         ,"--config="++topupcfg
                         ,"--out=build/2_topup/topup_Pos_Neg_b0"
                         ,"-v"]

    -- Preprocessing

    ["build/1_preproc/Pos.bval",
     "build/1_preproc/Neg.bval",
     "build/1_preproc/PosNeg.bval"]
      *>> \[posOut,negOut,posnegOut] -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        let posbvals = map (tobval._dwi._pos) dwipairs
            negbvals = map (tobval._dwi._neg) dwipairs
        need $ posbvals ++ negbvals
        posbvalues <- concat <$> traverse readbval posbvals
        negbvalues <- concat <$> traverse readbval negbvals
        writebval posOut $ posbvalues
        writebval negOut $ negbvalues
        writebval posnegOut $ posbvalues ++ negbvalues

    ["build/1_preproc/Pos.bvec",
     "build/1_preproc/Neg.bvec",
     "build/1_preproc/PosNeg.bvec"]
      *>> \[posOut,negOut,posnegOut] -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        let posbvecs = map (tobvec._dwi._pos) dwipairs
            negbvecs = map (tobvec._dwi._neg) dwipairs
        need $ posbvecs ++ negbvecs
        posvectors <- concat <$> traverse readbvec posbvecs
        negvectors <- concat <$> traverse readbvec negbvecs
        writebvec posOut $ posvectors
        writebvec negOut $ negvectors
        writebvec posnegOut $ posvectors ++ negvectors

    "build/1_preproc/PosNeg.nii.gz"
      %> \out -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        let dwis = map (_dwi._pos) dwipairs ++ map (_dwi._neg) dwipairs
        need dwis
        mergeVols out dwis
        trimVol out

    ["build/1_preproc/Pos_B0.nii.gz",
     "build/1_preproc/Neg_B0.nii.gz",
     "build/1_preproc/B0s.nii.gz"]
      *>> \[pos_b0, neg_b0, b0s] -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        need $ map (_dwi._pos) dwipairs ++ map (_dwi._neg) dwipairs
        writeB0s pos_b0 (map _pos dwipairs)
        writeB0s neg_b0 (map _neg dwipairs)
        mergeVols b0s [pos_b0, neg_b0]

    ["build/1_preproc/Pos_SeriesVolNum.txt",
     "build/1_preproc/Neg_SeriesVolNum.txt"]
      *>> \[posseries, negseries] -> do
        need [summaryYaml]
        Just ps <- liftIO $ decodeFile summaryYaml
        let
          minsizes = zipWith min (map (_size._pos) $ ps) (map (_size._neg) $ ps)
          seriesPos = zipWith printline minsizes $ map (_size._pos) ps
          seriesNeg = zipWith printline minsizes $ map (_size._neg) ps
          printline x y = printf "%d %d" x y
        writeFile' posseries $ unlines seriesPos
        writeFile' negseries $ unlines seriesNeg

    "build/1_preproc/index.txt"
      %> \out -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        writeFile' out (unlines $ map show $ mkIndexList dwipairs)

    "build/1_preproc/acqparams.txt"
      %> \out -> do
        need [summaryYaml]
        Just dwipairs <- liftIO $ decodeFile summaryYaml
        Just phasedir <- fmap read <$> getConfig "phasedir"
        Just echospacing <- fmap read <$> getConfig "echospacing"
        let dwi0 = _dwi._pos.head $ dwipairs
        need [dwi0]
        phaselength <- case phasedir of
                         PA -> read . fromStdout <$> command [] "fslval" [dwi0, "dim1"]
                         _ -> read . fromStdout <$> command [] "fslval" [dwi0, "dim2"]
        let readout = printf "%.6f" $ readoutTime phaselength echospacing
            numB0sToUse = sum . concatMap _b0indicesToUse
            acqParamsPos =  case phasedir of
              PA -> "0 1 0 " ++ readout
              RL -> "1 0 0 " ++ readout
            acqParamsNeg = case phasedir of
              PA -> "0 -1 0 " ++ readout
              RL -> "-1 0 0 " ++ readout
            acq = replicate (numB0sToUse $ map _pos dwipairs) acqParamsPos
            acq' = replicate (numB0sToUse $ map _neg dwipairs) acqParamsNeg
        writeFile' out $ unlines (acq ++ acq')

    "build/0_normalized/dwipairs.yaml"
      %> \out -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        dwipairs <- traverse readDWIPair $ zip3 [1..] posdwis negdwis
        let updatePath dwiinfo@DWIInfo{_dwi=dwi,_pid=pid,_dirType=dirType}
              = dwiinfo {_dwi=dwinew}
              where
                dwinew = printf "build/0_normalized/%s-%i.nii.gz" (show dirType) pid
            posNew = map (updatePath._pos) dwipairs
            negNew = map (updatePath._neg) dwipairs
        liftIO $ encodeFile out $ zipWith DWIPair posNew negNew

    ["build/0_normalized/*.nii.gz",
     "build/0_normalized/*.bval",
     "build/0_normalized/*.bvec"]
      *>> \[dwiOut, bvalOut, bvecOut] ->
        let
          meanfile = "build/0_normalized/Pos-1-meanb0"
          [dirtype, pid] = splitOn "-" $ takeBaseName' dwiOut
          process key pid = do
            Just dwis <- fmap words <$> getConfig key
            let dwiSrc = dwis !! (pid-1)
            need [dwiSrc, tobval dwiSrc, tobvec dwiSrc, meanfile]
            mean0 <- read <$> readFile' meanfile
            scaleDWI dwiOut dwiSrc (tobval dwiSrc) mean0
            copyFile' (tobval dwiSrc) bvalOut
            copyFile' (tobvec dwiSrc) bvecOut
        in
          case dirtype of
            "Pos" -> process "posdwis" (read pid)
            "Neg" -> process "negdwis" (read pid)
            _ -> error "This rule builds dwi's with format e.g. Pos-1.nii.gz"

    "build/0_normalized/Pos-1-meanb0"
      %> \out -> do
        Just posdwi0 <- fmap (head . words) <$> getConfig "posdwis"
        need [posdwi0, tobval posdwi0]
        mean0 <- getB0sMean posdwi0 (tobval posdwi0)
        writeFile' out $ show mean0
