module Main where

-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import           Data.Csv (encodeDefaultOrderedByName)
import           Development.Shake
import           Development.Shake.Config
import           FSL
import           Preproc
import           Text.Printf
import Data.Yaml (encodeFile, decodeFile)
import Data.Maybe
import System.FilePath

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    usingConfigFile "hcp.cfg"
    want ["build/eddy/eddy_unwarped_images.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

-- Post Eddy

    ["build/data/data.nii.gz",
     "build/data/bvals",
     "build/data/bvecs"]
      *>> \[out,_,_] -> do
      let deps@[summaryYaml
               ,eddyimages
               ,posseries
               ,negseries
               ,posbval
               ,posbvec
               ,negbval
               ,negbvec] =
               ["build/preproc/summary.yaml"
               ,"build/eddy/eddy_unwarped_images.nii.gz"
               ,"build/preproc/Pos_SeriesVolNum.txt"
               ,"build/preproc/Neg_SeriesVolNum.txt"
               ,"build/preproc/Pos.bval"
               ,"build/preproc/Pos.bvec"
               ,"build/preproc/Neg.bval"
               ,"build/preproc/Neg.bvec"
               ]
      need deps
      dwipairs <- fmap fromJust $ liftIO $ decodeFile summaryYaml
      let numPos = show $ sum $ map (_size.pos) dwipairs
          numNeg = show $ sum $ map (_size.neg) dwipairs
      withTempFile $ \eddypos ->
        withTempFile $ \eddyneg -> do
          unit $ command [] "fslroi" [eddyimages, eddypos, "0", numPos]
          unit $ command [] "fslroi" [eddyimages, eddyneg, numPos, numNeg]
          unit $ command [] "eddy_combine"
            [eddypos, posbval, posbvec, posseries
            ,eddyneg, negbval, negbvec, negseries
            ,takeDirectory out, "1"]

    ["build/data/nodif_brain.nii.gz",
     "build/data/nodif_brain_mask.nii.gz",
     "build/data/nodif.nii.gz"]
      *>> \[_,_,nodif] -> do
      let dat = "build/data/data.nii.gz"
      need [dat]
      unit $ command [] "fslmaths" [dat, "-thr", dat]
      unit $ command [] "bet" [dat , "build/data/nodif_brain" , "-m" , "-f" , "0.1"]
      unit $ command [] "fslroi" [dat, nodif, "0", "1"]


      -- Eddy

    "build/eddy/eddy_unwarped_images.nii.gz"
      %> \out -> do
        let deps@[vol,mask,index,acqp,bvec,bval,_,_] =
                ["build/preproc/PosNeg.nii.gz"
                ,"build/topup/nodif_brain_mask.nii.gz"
                ,"build/preproc/index.txt"
                ,"build/preproc/acqparams.txt"
                ,"build/preproc/PosNeg.bvec"
                ,"build/preproc/PosNeg.bval"
                ,"build/topup/topup_Pos_Neg_b0_fieldcoef.nii.gz"
                ,"build/topup/topup_Pos_Neg_b0_movpar.txt"]
        need deps
        unit $ command [] "eddy" ["--imain="++vol
                                ,"--mask="++mask
                                ,"--index="++index
                                ,"--acqp="++acqp
                                ,"--bvecs="++bvec
                                ,"--bvals="++bval
                                ,"--fwhm=0"
                                ,"--topup=build/topup/topup_Pos_Neg_b0"
                                ,"--flm=quadratic"
                                ,"-v"
                                ,"--out="++out]

      -- Topup

    ["build/topup/nodif_brain.nii.gz",
     "build/topup/nodif_brain_mask.nii.gz"]
      *>> \[out,_] -> do
      let hifib0 = "build/topup/hifib0.nii.gz"
      need [hifib0]
      command [] "bet" [hifib0, out, "-m", "-f", "0.2"]

    "build/topup/hifib0.nii.gz"
      %> \out -> do
      let deps@[posb0,negb0,_,_,params] =
            ["build/preproc/Pos_B0.nii.gz"
            ,"build/preproc/Neg_B0.nii.gz"
            ,"build/topup/topup_Pos_Neg_b0_fieldcoef.nii.gz"
            ,"build/topup/topup_Pos_Neg_b0_movpar.txt"
            ,"build/preproc/acqparams.txt"]
      need deps
      dimt <- (+1) <$> getDim4 posb0
      posb01 <- extractVol posb0 1
      negb01 <- extractVol negb0 1
      unit $ command [] "applytopup" [printf "--imain=%s,%s" posb01 negb01
                                     ,"--topup=build/topup/topup_Pos_Neg_b0"
                                     ,"--datain="++params
                                     ,"--inindex=1,"++ show dimt
                                     ,"--out="++out]
      liftIO $ removeFiles "." [posb01,negb01]

    ["build/topup/topup_Pos_Neg_b0_fieldcoef.nii.gz",
     "build/topup/topup_Pos_Neg_b0_movpar.txt"]
      *>> \_ -> do
      let deps@[b0s, acqparams, topupcfg] =
            ["build/preproc/B0s.nii.gz"
            ,"build/preproc/acqparams.txt"
            ,"b02b0.cnf"]
      need deps
      command [] "topup" ["--imain="++b0s
                         ,"--datain="++acqparams
                         ,"--config="++topupcfg
                         ,"--out=build/topup/topup_Pos_Neg_b0"
                         ,"-v"]

    -- Preprocessing

    ["build/preproc/Pos.bval",
     "build/preproc/Neg.bval",
     "build/preproc/PosNeg.bval"]
      *>> \[posOut,negOut,posnegOut] -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        posbvals <- concat <$> traverse (readbval . tobval) posdwis
        negbvals <- concat <$> traverse (readbval . tobval) negdwis
        writebval posOut $ posbvals
        writebval negOut $ negbvals
        writebval posnegOut $ posbvals ++ negbvals

    ["build/preproc/Pos.bvec",
     "build/preproc/Neg.bvec",
     "build/preproc/PosNeg.bvec"]
      *>> \[posOut,negOut,posnegOut] -> do
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        posbvecs <- concat <$> traverse (readbvec . tobvec) posdwis
        negbvecs <- concat <$> traverse (readbvec . tobvec) negdwis
        writebvec posOut $ posbvecs
        writebvec negOut $ negbvecs
        writebvec posnegOut $ posbvecs ++ negbvecs

    ["build/preproc/PosNeg.nii.gz",
     "build/preproc/Pos_B0.nii.gz",
     "build/preproc/Neg_B0.nii.gz",
     "build/preproc/B0s.nii.gz",
     "build/preproc/acqparams.txt",
     "build/preproc/index.txt",
     "build/preproc/summary.yaml",
     "build/preproc/Pos_SeriesVolNum.txt",
     "build/preproc/Neg_SeriesVolNum.txt"]
      *>> \[outvol, pos_b0, neg_b0, b0s, acqparams, outindex, summaryJson,
            posseries, negseries] -> do
        let
          readDWIPair (dwi, dwi') = mkDWIPair <$> pure dwi <*>
                                                  pure dwi' <*>
                                                  readbval (tobval dwi) <*>
                                                  readbval (tobval dwi')
          -- toRecords (DWIPair i i') = [i,i']
        Just posdwis <- fmap words <$> getConfig "posdwis"
        Just negdwis <- fmap words <$> getConfig "negdwis"
        need $ posdwis ++ negdwis
        ps <- traverse readDWIPair $ zip posdwis negdwis
        -- writeFile' summaryCsv $ B.unpack $ encodeDefaultOrderedByName (concatMap toRecords ps)
        liftIO $ encodeFile summaryJson ps
        writeIndex outindex ps
        let
          seriesPos = zipWith (\x y -> printf "%d %d" x y) minsizes $ map (_size.pos) ps
          seriesNeg = zipWith (\x y -> printf "%d %d" x y) minsizes $ map (_size.neg) ps
          minsizes = zipWith min (map (_size.pos) $ ps) (map (_size.neg) $ ps)
        writeFile' posseries $ unlines seriesPos
        writeFile' negseries $ unlines seriesNeg
        Just phasedir <- fmap read <$> getConfig "phase"
        Just echospacing <- fmap read <$> getConfig "echospacing"
        writeAcqparams acqparams phasedir echospacing ps
        writeB0s pos_b0 (map pos ps)
        writeB0s neg_b0 (map neg ps)
        mergeVols b0s [pos_b0, neg_b0]
        writeCombined outvol ps
