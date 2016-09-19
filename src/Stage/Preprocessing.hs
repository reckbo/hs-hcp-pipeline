module Stage.Preprocessing
(  posbval
 , negbval
 , posnegbval
 , posbvec
 , negbvec
 , posnegbvec
 , posNegVol
 , posb0s
 , negb0s
 , posnegb0s
 , posseries_txt
 , negseries_txt
 , index_txt
 , acqparams_txt
 , rules
 ) where

import           Data.Yaml                  (decodeFile)
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           FSL                        (mergeVols, readbval, readbvec,
                                             tobval, tobvec, trimVol, writebval,
                                             writebvec)
import qualified Stage.Normalize            as Normalize
import           Text.Printf
import           Types                      (DWIInfo (..), DWIPair (..),
                                             PhaseDirection (..))
import           Util                       (mkIndexList, readoutTime, writeB0s)

outdir :: [Char]
outdir = "hcp-output/1_preproc"
posbval :: FilePath
posbval = outdir </> "Pos.bval"
negbval :: FilePath
negbval = outdir </> "Neg.bval"
posnegbval :: FilePath
posnegbval = outdir </> "PosNeg.bval"
posbvec :: FilePath
posbvec = outdir </> "Pos.bvec"
negbvec :: FilePath
negbvec = outdir </> "Neg.bvec"
posnegbvec :: FilePath
posnegbvec = outdir </> "PosNeg.bvec"
posNegVol :: FilePath
posNegVol = outdir </> "PosNeg.nii.gz"
posb0s :: FilePath
posb0s = outdir </> "Pos_b0s.nii.gz"
negb0s :: FilePath
negb0s = outdir </> "Neg_b0s.nii.gz"
posnegb0s :: FilePath
posnegb0s = outdir </> "PosNeg_b0s.nii.gz"
posseries_txt :: FilePath
posseries_txt = outdir </> "Pos_SeriesVolNum.txt"
negseries_txt :: FilePath
negseries_txt = outdir </> "Neg_SeriesVolNum.txt"
index_txt :: FilePath
index_txt = outdir </> "index.txt"
acqparams_txt :: FilePath
acqparams_txt = outdir </> "acqparams.txt"

rules :: Rules ()
rules = do

    [posbval,
     negbval,
     posnegbval]
      *>> \[posOut,negOut,posnegOut] -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        let posbvals = map (tobval._dwi._pos) dwipairs
            negbvals = map (tobval._dwi._neg) dwipairs
        need $ posbvals ++ negbvals
        posbvalues <- concat <$> traverse readbval posbvals
        negbvalues <- concat <$> traverse readbval negbvals
        writebval posOut $ posbvalues
        writebval negOut $ negbvalues
        writebval posnegOut $ posbvalues ++ negbvalues

    [posbvec,
     negbvec,
     posnegbvec]
      *>> \[posOut,negOut,posnegOut] -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        let posbvecs = map (tobvec._dwi._pos) dwipairs
            negbvecs = map (tobvec._dwi._neg) dwipairs
        need $ posbvecs ++ negbvecs
        posvectors <- concat <$> traverse readbvec posbvecs
        negvectors <- concat <$> traverse readbvec negbvecs
        writebvec posOut $ posvectors
        writebvec negOut $ negvectors
        writebvec posnegOut $ posvectors ++ negvectors

    posNegVol
      %> \out -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        let dwis = map (_dwi._pos) dwipairs ++ map (_dwi._neg) dwipairs
        need dwis
        mergeVols out dwis
        trimVol out

    [posb0s,
     negb0s,
     posnegb0s]
      *>> \_ -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        need $ map (_dwi._pos) dwipairs ++ map (_dwi._neg) dwipairs
        writeB0s posb0s (map _pos dwipairs)
        writeB0s negb0s (map _neg dwipairs)
        mergeVols posnegb0s [posb0s, negb0s]

    [posseries_txt,
     negseries_txt]
      *>> \[posseries, negseries] -> do
        need [Normalize.dwipairs_yaml]
        Just ps <- liftIO $ decodeFile Normalize.dwipairs_yaml
        let
          minsizes = zipWith min (map (_size._pos) $ ps) (map (_size._neg) $ ps)
          seriesPos = zipWith printline minsizes $ map (_size._pos) ps
          seriesNeg = zipWith printline minsizes $ map (_size._neg) ps
          printline x y = printf "%d %d" x y
        writeFile' posseries $ unlines seriesPos
        writeFile' negseries $ unlines seriesNeg

    index_txt
      %> \out -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        writeFile' out (unlines $ map show $ mkIndexList dwipairs)

    acqparams_txt
      %> \out -> do
        need [Normalize.dwipairs_yaml]
        Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
        Just phasedir <- fmap read <$> getConfig "phasedir"
        Just echospacing <- fmap read <$> getConfig "echospacing"
        let dwi0 = _dwi._pos.head $ dwipairs
        need [dwi0]
        phaselength <- case phasedir of
                         PA -> read . fromStdout <$> command [] "fslval" [dwi0, "dim1"]
                         _ -> read . fromStdout <$> command [] "fslval" [dwi0, "dim2"]
        let readout = printf "%.6f" $ readoutTime phaselength echospacing
            numB0sToUse = length . concatMap _b0indicesToUse
            acqParamsPos =  case phasedir of
              PA -> "0 1 0 " ++ readout
              RL -> "1 0 0 " ++ readout
            acqParamsNeg = case phasedir of
              PA -> "0 -1 0 " ++ readout
              RL -> "-1 0 0 " ++ readout
            acq = replicate (numB0sToUse $ map _pos dwipairs) acqParamsPos
            acq' = replicate (numB0sToUse $ map _neg dwipairs) acqParamsNeg
        writeFile' out $ unlines (acq ++ acq')
