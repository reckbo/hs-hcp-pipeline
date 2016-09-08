module FSL
    (
      BValue (..),
      replaceExtension',
      takeBaseName',
      extractVol_,
      extractVol,
      extractVols_,
      extractVols,
      mergeVols,
      trimVol,
      getDim3,
      getDim4,
      readbval,
      tobval,
      writebval,
      tobvec,
      readbvec,
      writebvec
    ) where

import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Printf

newtype BValue = BValue Int
  deriving (Eq, Ord)

instance Show BValue where
  show (BValue b) = show b

data Vec = Vec { vx::Double,
                 vy::Double,
                 vz::Double }

instance Show Vec where
  show (Vec v1 v2 v3) = printf "%f %f %f" v1 v2 v3

trim :: String -> String
trim = unwords . words

takeBaseName' :: FilePath -> String
takeBaseName' = takeBaseName . takeBaseName

replaceExtension' :: FilePath -> String -> FilePath
replaceExtension' f ext = replaceExtension (dropExtension f) ext

dropExtension' :: FilePath -> FilePath
dropExtension' = dropExtension . dropExtension

insertSuffix :: FilePath -> String -> FilePath
insertSuffix f suff = (++".nii.gz") . (++suff) . dropExtension' $ f

trimVol :: FilePath -> Action ()
trimVol dwi = do
  dim3 <- getDim3 dwi
  when (odd dim3) $ trimVol' dwi
  where
    trimVol' d = withTempFile $ \tmpfile -> do
      putNormal "DWI's have odd number of z-slices, remove one to make even"
      copyFile' d tmpfile
      command [] "fslroi" $ [tmpfile,dwi] ++ map show ([0,-1,0,-1,1,-1] :: [Int])

getDim3 :: FilePath -> Action Int
getDim3 = fmap read . fslval "dim3"

getDim4 :: FilePath -> Action Int
getDim4 = fmap read . fslval "dim4"

fslval :: String -> FilePath -> Action String
fslval key dwi = trim . fromStdout <$> command [] "fslval" [dwi, key]

tobval :: FilePath -> FilePath
tobval f = replaceExtension' f "bval"

readbval :: FilePath -> Action [BValue]
readbval f = map (BValue . read) . words <$> readFile' f

writebval :: FilePath -> [BValue] -> Action ()
writebval out arr = writeFile' out (unwords . map show $ arr)

tobvec :: FilePath -> FilePath
tobvec f = replaceExtension' f "bvec"

-- Reads 3 x N
readbvec :: FilePath ->  Action [Vec]
readbvec f = toVecs <$> map toArr <$> readFileLines f
  where
    toVecs [v1,v2,v3] = zipWith3 Vec v1 v2 v3
    toVecs _ = error $ "Seems to be an invalid bvecs file: " ++ f
    toArr = map read . words

writebvec :: FilePath -> [Vec] -> Action ()
writebvec f dirs = writeFile' f $ unlines [vx',vy',vz']
  where vx' = unwords . map (show . vx) $ dirs
        vy' = unwords . map (show . vy) $ dirs
        vz' = unwords . map (show . vz) $ dirs

extractVol_ :: FilePath -> FilePath -> Int -> Action ()
extractVol_ out dwi idx = command [] "fslroi" [dwi, out, show idx, "1"]

extractVol :: FilePath -> Int -> Action FilePath
extractVol dwi idx = out <$ extractVol_ out dwi idx
  where
    out = insertSuffix dwi (printf "-%03d" idx)

extractVols :: FilePath -> [Int] -> Action FilePath
extractVols dwi idx
  = withTempFile $ \tmpfile -> extractVols_ tmpfile dwi idx >> return tmpfile
    -- out = replaceExtension' dwi (printf "%s.nii.gz" (intercalate "-" $ map show idx))

extractVols_ :: FilePath -> FilePath -> [Int] -> Action ()
extractVols_ out dwi idx = traverse (extractVol dwi) idx >>= mergeVols out

mergeVols :: FilePath -> [FilePath] -> Action ()
mergeVols out vols = unit $ command [] "fslmerge" (["-t", out] ++ vols)
