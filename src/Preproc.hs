{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Preproc
  (   DWIInfo (..)
    , DWIPair (..)
    , PhaseDirection (..)
    , DirType (..)
    , mkDWIPair
    , readoutTime
    , writeB0s
    , mkIndexList
    , readDWIPair
    , getB0sMean
    , scaleDWI
  ) where

import           Data.Function
import           Data.List
import           Data.Yaml
import           Development.Shake
import           FSL
import           GHC.Generics

data DirType = Pos | Neg
  deriving (Show, Generic)

data DWIInfo = DWIInfo
    {_pid                  :: Int
    ,_dirType              :: DirType
    ,_dwi                  :: FilePath
    ,_size                 :: Int
    ,_b0indices            :: [Int]
    ,_b0indicesWithMinDist :: [Int]
    ,_b0indicesToUse       :: [Int]
    }
  deriving (Show, Generic)

-- instance ToField [Int] where
--   toField = toField . unwords . map show
-- instance ToNamedRecord DWIInfo
-- instance FromNamedRecord DWIInfo
-- instance DefaultOrdered DWIInfo

instance ToJSON DirType
instance ToJSON DWIInfo
instance FromJSON DWIInfo
instance FromJSON DirType

data DWIPair = DWIPair
  { _pos :: DWIInfo
  , _neg :: DWIInfo }
  deriving (Show, Generic)

instance ToJSON DWIPair
instance FromJSON DWIPair

type EchoSpacing = Float
type PhaseLength = Int
type DWI = FilePath

b0maxbval :: BValue
b0maxbval = BValue 50

b0dist :: Int
b0dist = 45

data PhaseDirection = RL | PA
  deriving (Show, Read)

readDWIPair :: (Int, DWI, DWI) -> Action DWIPair
readDWIPair (pid, dwi, dwi') = mkDWIPair <$>
                                pure pid <*>
                                pure dwi <*>
                                pure dwi' <*>
                                readbval (tobval dwi) <*>
                                readbval (tobval dwi')

-- readPhaseLength :: PhaseDirection -> FilePath -> Action PhaseLength
-- readPhaseLength pedir dwi = case pedir of
--   PA -> read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
--   _   -> read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

getValidB0Indices :: [BValue] -> [Int]
getValidB0Indices bs = reverse $ foldl' f [i0] indices
  where
    f (i:is) i' = if (i' - i) >= b0dist
                     then i':i:is
                     else i:is
    f _ _ = error "getValideB0Indices: DWI must have at least two b-values."
    (i0:indices) = findIndices (< b0maxbval) bs

mkDWIInfo :: Int -> DirType -> DWI -> [BValue] -> Int -> DWIInfo
mkDWIInfo pid dirtype dwi bs matchingLength
  = DWIInfo
  pid
  dirtype
  dwi
  (length bs)
  (findIndices (< b0maxbval) bs)
  (getValidB0Indices bs)
  (filter (< matchingLength) $ getValidB0Indices bs)

mkDWIPair :: Int -> DWI -> DWI -> [BValue] -> [BValue] -> DWIPair
mkDWIPair pid dwi dwi' bs bs'
  = DWIPair
  (mkDWIInfo pid Pos dwi bs matchingLength)
  (mkDWIInfo pid Neg dwi' bs' matchingLength)
  where matchingLength = (min`on`length) bs bs'
-- dwiPairsFromCsv :: String -> [DWIPair]
-- dwiPairsFromCsv csv =
--   let
--     infos = fmap toList $ decode $ B.pack csv
--   in
--     case infos of
--       (Left msg) -> error msg
--       Right infos -> map (\[i,i'] -> DWIPair i i') $ groupBy ((==)`on`_pairId) infos

writeB0s :: FilePath -> [DWIInfo] -> Action ()
writeB0s out dwiinfos =
  do fs <- traverse writeB0 dwiinfos
     mergeVols out fs
     trimVol out
  where
    writeB0 dwiinfo = extractVols (_dwi dwiinfo) (_b0indicesToUse dwiinfo)

-- writeAcqparams :: FilePath -> PhaseDirection -> EchoSpacing -> [DWIPair] -> Action ()
-- writeAcqparams out phasedir echo dwipairs = do
--   phaselength <- readPhaseLength phasedir (_dwi . _pos . head $ dwipairs)
--   let
--     readout = printf "%.6f" $ readoutTime phaselength echo
--     (p, n) = (numValidB0s _pos dwipairs, numValidB0s _neg dwipairs)
--     acqParamsPos =  case phasedir of
--       PA -> "0 1 0 " ++ readout
--       RL -> "1 0 0 " ++ readout
--     acqParamsNeg = case phasedir of
--       PA -> "0 -1 0 " ++ readout
--       RL -> "-1 0 0 " ++ readout
--     acq = replicate p acqParamsPos
--     acq' = replicate n acqParamsNeg
--   writeFile' out $ unlines (acq ++ acq')

-- writeIndexList :: FilePath -> [DWIPair] -> Action ()
-- writeIndexList out dwipairs = writeFile' out (unlines $ map show $ mkIndex dwipairs)

-- dwis :: [DWIPair] -> [FilePath]
-- dwis dwipairs = (map (_dwi._pos) dwipairs) ++ (map (_dwi._neg) dwipairs)

mkIndexList :: [DWIPair] -> [Int]
mkIndexList dwipairs = mkIndex' $ addLast b0indices size
  where
    posSizes = map (_size . _pos) dwipairs
    negSizes = map (_size . _neg) dwipairs
    sizes = scanl (+) 0 $ posSizes ++ negSizes
    size = head . reverse $ sizes
    posb0indices = map (_b0indicesToUse . _pos) dwipairs
    negb0indices = map (_b0indicesToUse . _neg) dwipairs
    b0indices = concat $ zipWith (\is sz -> map (+sz) is) (posb0indices++negb0indices) sizes
    mkIndex' is = reverse $ foldl g [] is
      where g res i =
              let dx = i - length res
                  val = case res of
                    [] -> 1
                    _ -> 1 + head res
              in (replicate dx val) ++ res

-- writeCombined :: FilePath -> [DWIPair] -> Action ()
-- writeCombined out dwipairs = do
--   mergeVols out $ map (_dwi . _pos) dwipairs ++ map (_dwi . _neg) dwipairs
--   trimVol out

addLast :: [a] -> a -> [a]
addLast xs y = reverse . (y:) . reverse $ xs

getB0sMean :: FilePath -> FilePath -> Action Float
getB0sMean dwi bval = do
  b0indices <- findIndices (< b0maxbval) <$> readbval bval
  withTempFile $ \b0s -> do
    extractVols_ b0s dwi b0indices
    command_ [] "fslmaths" [b0s, "-Tmean", b0s]
    Stdout mean <- command [] "fslmeants" ["-i", b0s]
    return $ read mean

scaleDWI :: FilePath -> FilePath -> FilePath -> Float -> Action ()
scaleDWI out src srcBval mean0 = do
  mean <- getB0sMean src srcBval
  command_ [] "fslmaths" [src
                         ,"-mul", show mean0
                         ,"-div", show mean
                         ,out]

-- normalize :: Float -> DWIInfo -> Action DWIInfo
-- normalize mean0 dwiinfo@DWIInfo{_dwi=dwi, _pid=pid, _dirType=dirType} =
--     case (pid,dirType) of
--       (1, Pos) -> do copyFile' dwi dwinew
--                      return $ dwiinfo {_dwi=dwinew}
--       _ -> do scaleDWI dwinew mean0 dwiinfo
--               copyFile' (tobval $ dwi) (tobval dwinew)
--               copyFile' (tobvec $ dwi) (tobvec dwinew)
--               return $ dwiinfo {_dwi=dwinew}
--     where
--       dwinew = printf "build/0_normalized/%s-%i.nii.gz" (show dirType) pid

-- normalizePair :: Float -> DWIPair -> Action DWIPair
-- normalizePair mean0 (DWIPair pos neg) = do
--     pos' <- normalize mean0 pos
--     neg' <- normalize mean0 neg
--     return $ DWIPair pos' neg'
