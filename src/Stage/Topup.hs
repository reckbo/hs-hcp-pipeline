module Stage.Topup
  (  nodif_brain_mask
   , outprefix
   , fieldcoef
   , movpar_txt
   , rules
  ) where

import           Development.Shake
import           Development.Shake.FilePath
import           FSL                        (extractVol_, getDim4)
import qualified Stage.Preprocessing        as Preprocessing
import           Text.Printf

outdir :: [Char]
outdir = "hcp-output/2_topup"

nodif_brain :: FilePath
nodif_brain = outdir </> "nodif_brain.nii.gz"
nodif_brain_mask :: FilePath
nodif_brain_mask= outdir </> "nodif_brain_mask.nii.gz"
hifib0 :: FilePath
hifib0 = outdir </> "hifib0.nii.gz"

outprefix :: FilePath
outprefix = outdir </> "topup_Pos_Neg_b0"
fieldcoef :: [Char]
fieldcoef = outprefix ++ "_fieldcoef.nii.gz"
movpar_txt :: [Char]
movpar_txt = outprefix ++  "_movpar.txt"


topupcfg :: [Char]
topupcfg  = "b02b0.cnf"

rules :: Rules ()
rules = do
    [nodif_brain,
     nodif_brain_mask]
      *>> \_ -> do
      need [hifib0]
      command [] "bet" [hifib0, nodif_brain, "-m", "-f", "0.2"]

    hifib0
      %> \_ -> do
      need [Preprocessing.posb0s
           ,Preprocessing.negb0s
           ,Preprocessing.acqparams_txt
           ,fieldcoef
           ,movpar_txt]
      dimt <- (+1) <$> getDim4 Preprocessing.posb0s
      withTempFile $ \posb01 ->
        withTempFile $ \negb01 -> do
          extractVol_ posb01 Preprocessing.posb0s 1
          extractVol_ negb01 Preprocessing.negb0s 1
          command_ [] "applytopup" [printf "--imain=%s,%s" posb01 negb01
                                 ,"--topup=" ++ outprefix
                                 ,"--datain="++Preprocessing.acqparams_txt
                                 ,"--inindex=1,"++ show dimt
                                 ,"--out="++hifib0]

    [fieldcoef,
     movpar_txt]
      *>> \_ -> do
      need [Preprocessing.posnegb0s
           ,Preprocessing.acqparams_txt
           ,topupcfg]
      command [] "topup" ["--imain="++Preprocessing.posnegb0s
                         ,"--datain="++Preprocessing.acqparams_txt
                         ,"--config="++topupcfg
                         ,"--out=" ++ outprefix
                         ,"-v"]
