{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Stage.Normalize
  (
    dwipairs_yaml,
    meanb0_file,
    rules
  )
  where

import           Data.List.Split            (splitOn)
import           Data.Yaml                  (encodeFile)
import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath
import           FSL                        (takeBaseName', tobval, tobvec)
import           Text.Printf
import           Types                      (DWIInfo (..), DWIPair (..))
import           Util                       (getB0sMean, readDWIPair, scaleDWI)

outdir :: [Char]
outdir = "hcp-output/0_normalized"

dwipairs_yaml :: FilePath
dwipairs_yaml = outdir </> "dwipairs.yaml"

meanb0_file :: FilePath
meanb0_file = outdir </> "Pos-1-meanb0"

rules :: Rules ()
rules = do

    dwipairs_yaml
      %> \_ -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        dwipairs <- traverse readDWIPair $ zip3 [1..] posdwis negdwis
        let updatePath dwiinfo@DWIInfo{_pid=pid,_dirType=dirType}
              = dwiinfo {_dwi=dwinew}
              where
                dwinew = printf (outdir </> "%s-%i.nii.gz") (show dirType) pid
            posNew = map (updatePath._pos) dwipairs
            negNew = map (updatePath._neg) dwipairs
        liftIO $ encodeFile dwipairs_yaml $ zipWith DWIPair posNew negNew

    [outdir </> "*.nii.gz",
     outdir </> "*.bval",
     outdir </> "*.bvec"]
      *>> \[dwiOut, bvalOut, bvecOut] ->
        let
          [dirtype, pid] = splitOn "-" $ takeBaseName' dwiOut
          process key pid = do
            Just dwis <- fmap words <$> getConfig key
            let dwiSrc = dwis !! (pid-1)
            need [dwiSrc, tobval dwiSrc, tobvec dwiSrc, meanb0_file]
            mean0 <- read <$> readFile' meanb0_file
            scaleDWI dwiOut dwiSrc (tobval dwiSrc) mean0
            copyFile' (tobval dwiSrc) bvalOut
            copyFile' (tobvec dwiSrc) bvecOut
        in
          case dirtype of
            "Pos" -> process "posdwis" (read pid)
            "Neg" -> process "negdwis" (read pid)
            _ -> error "This rule builds dwi's with format e.g. Pos-1.nii.gz"

    meanb0_file
      %> \_ -> do
        Just posdwi0 <- fmap (head . words) <$> getConfig "posdwis"
        need [posdwi0, tobval posdwi0]
        mean0 <- getB0sMean posdwi0 (tobval posdwi0)
        writeFile' meanb0_file $ show mean0
