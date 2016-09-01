module FSL
    (
      BValue (..),
      replaceExtension',
      extractVol,
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

import           Development.Shake
import           Development.Shake.FilePath
import           Text.Printf
import Data.List
import Control.Monad

newtype BValue = BValue Int
  deriving (Show, Eq, Ord)


data Vec = Vec { vx::Double,
                 vy::Double,
                 vz::Double }

-- readDWIBvec :: DWI -> Action [Dir]
-- readDWIBvec dwi = readbvec (replaceExtension' f "bvec")
--   where f = filepath dwi

instance Show Vec where
  show (Vec v1 v2 v3) = printf "%f %f %f" v1 v2 v3

trim :: String -> String
trim = unwords . words

replaceExtension' :: FilePath -> String -> FilePath
replaceExtension' f ext = replaceExtension (dropExtension f) ext

trimVol :: FilePath -> Action ()
trimVol dwi = do
  dim3 <- getDim3 dwi
  when (odd dim3) $ trimVol' dwi
  where
    trimVol' d = withTempFile $ \tmpfile -> do
      putNormal "DWI's have odd number of z-slices, remove one to make even"
      copyFile' d tmpfile
      command [] "fslroi" $ [tmpfile,dwi] ++ map show ([0,-1,0,-1,1,-1] :: [Int])

-- numDirs :: FilePath -> Action Int
-- numDirs dwi = read <$> fslval "dim4" dwi

getDim3 :: FilePath -> Action Int
getDim3 = fmap read . fslval "dim3"

getDim4 :: FilePath -> Action Int
getDim4 = fmap read . fslval "dim4"

fslval :: String -> FilePath -> Action String
fslval key dwi = trim . fromStdout <$> command [] "fslval" [dwi, key]

tobval :: FilePath -> FilePath
tobval f = replaceExtension' f "bval"

tobvec :: FilePath -> FilePath
tobvec f = replaceExtension' f "bvec"

-- Reads 3 x N
readbvec :: FilePath ->  Action [Vec]
readbvec f = toVecs <$> map toArr <$> readFileLines f
  where
    toVecs [v1,v2,v3] = zipWith3 Vec v1 v2 v3
    toVecs _ = error $ "Seems to be an invalid bvecs file: " ++ f
    -- toVecs _ = Left $ "Seems to be an invalid bvecs file: " ++ f
    toArr = map read . words

writebvec :: FilePath -> [Vec] -> Action ()
writebvec f dirs = writeFile' f $ unlines [vx',vy',vz']
  where vx' = unwords . map (show . vx) $ dirs
        vy' = unwords . map (show . vy) $ dirs
        vz' = unwords . map (show . vz) $ dirs

-- mergebvecs :: FilePath -> [FilePath] -> Action ()
-- mergebvecs outbvec fs = do
--                   dirs <- readbvecs fs
--                   case dirs of
--                     Right xs -> writebvec outbvec xs
--                     Left msg -> error msg
--                 where
--                   readbvecs :: [FilePath] -> Action (Either String [Dir])
--                   readbvecs fs = (fmap concat . sequenceA) <$> traverse readbvec fs

-- readbvals :: [FilePath] -> Action [Int]
-- readbvals fs = concat <$> traverse readbval fs

-- mergebvals :: FilePath -> [FilePath] -> Action ()
-- mergebvals out = readbvals >=> writebval out

extractVol :: FilePath -> Int -> Action FilePath
extractVol dwi idx
  = outPath <$ (mycmd :: Action ())
    where
      mycmd = command [] "fslroi" [dwi, outPath, show idx, "1"]
      outPath = replaceExtension' dwi (printf "b0-%04d.nii.gz" idx)

readbval :: FilePath -> Action [BValue]
readbval f = map (BValue . read) . words <$> readFile' f

writebval :: FilePath -> [BValue] -> Action ()
writebval out arr = writeFile' out (unwords . map show $ arr)

extractVols :: FilePath -> [Int] -> Action FilePath
extractVols dwi idx
  = do
    fs <- traverse (extractVol dwi) idx
    mergeVols out fs
    return out
  where
    out = replaceExtension' dwi (printf "%s.nii.gz" (intercalate "-" $ map show idx))

mergeVols :: FilePath -> [FilePath] -> Action ()
mergeVols out vols = unit $ command [] "fslmerge" (["-t", out] ++ vols)
