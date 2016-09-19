module PostEddy
  ( datavol
  , nodif
  , rules
  ) where

import           Development.Shake
import           System.FilePath
import           Preproc
import           Data.Yaml                (decodeFile)
import qualified Normalize
import qualified Preprocessing
import qualified Eddy

outdir :: [Char]
outdir = "hcp-output/4_data"

nodif :: FilePath
nodif = outdir </> "nodif.nii.gz"

nodif_brain :: FilePath
nodif_brain = outdir </> "nodif_brain.nii.gz"

nodif_brain_mask :: FilePath
nodif_brain_mask = outdir </> "nodif_brain_mask.nii.gz"

datavol :: FilePath
datavol = outdir </> "data.nii.gz"

bvals :: FilePath
bvals = outdir </> "bvals"

bvecs :: FilePath
bvecs = outdir </> "bvecs"

rules :: Rules ()
rules = do

    [nodif_brain,
     nodif_brain_mask,
     nodif]
      *>> \_ -> do
      need [datavol]
      -- Remove negative intensity values (caused by spline interpolation) from final data
      command_ [] "fslmaths" [datavol, "-thr", "0", datavol]
      command_ [] "bet" [datavol , nodif_brain , "-m" , "-f" , "0.1"]
      command_ [] "fslroi" [datavol, nodif, "0", "1"]

    [datavol,
     bvals,
     bvecs]
      *>> \_ -> do
      need [Normalize.dwipairs_yaml
           ,Preprocessing.posseries_txt
           ,Preprocessing.negseries_txt
           ,Preprocessing.posbval
           ,Preprocessing.negbval
           ,Preprocessing.posbvec
           ,Preprocessing.negbvec
           ,Eddy.eddy_unwarped_images
           ]
      Just dwipairs <- liftIO $ decodeFile Normalize.dwipairs_yaml
      let numPos = show $ sum $ map (_size._pos) dwipairs
          numNeg = show $ sum $ map (_size._neg) dwipairs
      withTempFile $ \eddypos ->
        withTempFile $ \eddyneg -> do
          command_ [] "fslroi" [Eddy.eddy_unwarped_images, eddypos, "0", numPos]
          command_ [] "fslroi" [Eddy.eddy_unwarped_images, eddyneg, numPos, numNeg]
          command_ [] "eddy_combine" [eddypos, Preprocessing.posbval, Preprocessing.posbvec, Preprocessing.posseries_txt
                                     ,eddyneg, Preprocessing.negbval, Preprocessing.negbvec, Preprocessing.negseries_txt
                                     ,takeDirectory datavol, "1"]