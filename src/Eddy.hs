module Eddy
  ( eddy_unwarped_images
  , rules
  ) where

import Development.Shake
import Development.Shake.FilePath
import qualified Preprocessing
import qualified Topup

outdir :: [Char]
outdir = "hcp-output/3_eddy"
eddy_unwarped_images :: FilePath
eddy_unwarped_images = outdir </> "eddy_unwarped_images.nii.gz"

rules :: Rules ()
rules = do
  eddy_unwarped_images
    %> \out -> do
      need [Preprocessing.posNegVol
           ,Preprocessing.index_txt
           ,Preprocessing.acqparams_txt
           ,Preprocessing.posnegbvec
           ,Preprocessing.posnegbval
           ,Topup.nodif_brain_mask
           ,Topup.fieldcoef
           ,Topup.movpar_txt]
      command_ [] "eddy" ["--imain="++Preprocessing.posNegVol
                          ,"--mask="++Topup.nodif_brain_mask
                          ,"--index="++Preprocessing.index_txt
                          ,"--acqp="++Preprocessing.acqparams_txt
                          ,"--bvecs="++Preprocessing.posnegbvec
                          ,"--bvals="++Preprocessing.posnegbval
                          ,"--fwhm=0"
                          ,"--topup=" ++ Topup.outprefix
                          ,"--flm=quadratic"
                          ,"-v"
                          ,"--out="++out]

