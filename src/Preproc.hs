{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric        #-}
module Preproc
  (
    mkDWIPair
    , PhaseDirection (..)
    , writeB0s
    , writeCombined
    , writeIndex
    , writeAcqparams
    , DWIPair (..)

  ) where

import           Data.Csv
import           Data.Function
import           Data.List
import           Development.Shake
import           FSL
import           GHC.Generics

type EchoSpacing = Float
type PhaseLength = Int
type DWI = FilePath

b0maxbval :: BValue
b0maxbval = BValue 50

b0dist :: Int
b0dist = 45

-- acqParamsPAPos :: String
-- acqParamsPAPos = "0 1 0"
-- acqParamsPANeg :: String
-- acqParamsPANeg = "0 -1 0"
-- acqParamsRLPos :: String
-- acqParamsRLPos = "1 0 0"
-- acqParamsRLNeg :: String
-- acqParamsRLNeg = "-1 0 0"

data PhaseDirection = RL | PA
  deriving (Show, Read)

readPhaseLength :: PhaseDirection -> FilePath -> Action PhaseLength
readPhaseLength pedir dwi = case pedir of
  PA -> read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
  _   -> read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

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

mkDWIInfo :: Int -> DWI -> [BValue] -> Int -> DWIInfo
mkDWIInfo pid dwi bs matchingLength
  = DWIInfo
  pid
  dwi
  (length bs)
  (findIndices (< b0maxbval) bs)
  (getValidB0Indices bs)
  (filter (< matchingLength) $ getValidB0Indices bs)

mkDWIPair :: Int -> DWI -> DWI -> [BValue] -> [BValue] -> DWIPair
mkDWIPair pid dwi dwi' bs bs'
  = DWIPair
  (mkDWIInfo pid dwi bs matchingLength)
  (mkDWIInfo pid dwi' bs' matchingLength)
  where matchingLength = (min`on`length) bs bs'

data DWIInfo = DWIInfo
    {
     _pairId               :: Int
    ,_dwi                  :: FilePath
    ,_size                 :: Int
    ,_b0indices            :: [Int]
    ,_b0indicesWithMinDist :: [Int]
    ,_b0indicesToUse       :: [Int]
    }
  deriving (Show, Generic)

instance ToField [Int] where
  toField = toField . unwords . map show
instance ToNamedRecord DWIInfo
instance DefaultOrdered DWIInfo

data DWIPair = DWIPair { pos :: DWIInfo, neg :: DWIInfo }
  deriving Show

writeB0s :: FilePath -> [DWIInfo] -> Action ()
writeB0s out dwiinfos =
  do fs <- traverse writeB0 dwiinfos
     mergeVols out fs
  where
    writeB0 dwiinfo = extractVols (_dwi dwiinfo) (_b0indicesToUse dwiinfo)

numValidB0s :: (DWIPair -> DWIInfo) -> [DWIPair] -> Int
numValidB0s posneg xs = sum $  map (length . _b0indicesToUse . posneg) xs

writeAcqparams :: FilePath -> PhaseDirection -> EchoSpacing -> [DWIPair] -> Action ()
writeAcqparams out phasedir echo dwipairs = do
  phaselength <- readPhaseLength phasedir (_dwi . pos . head $ dwipairs)
  let
    readout = show $ readoutTime phaselength echo
    (p, n) = (numValidB0s pos dwipairs, numValidB0s neg dwipairs)
    acqParamsPos =  case phasedir of
      PA -> "0 1 0 " ++ readout
      RL -> "1 0 0 " ++ readout
    acqParamsNeg = case phasedir of
      PA -> "0 -1 0 " ++ readout
      RL -> "-1 0 0 " ++ readout
    acq = replicate p acqParamsPos
    acq' = replicate n acqParamsNeg
  writeFile' out $ unlines (acq ++ acq')

writeIndex :: FilePath -> [DWIPair] -> Action ()
writeIndex out dwipairs = writeFile' out (unlines $ indexPos ++ indexNeg)
  where
    numVols posneg = sum . map (_size . posneg)
    indexPos = replicate (numVols pos dwipairs) "0"
    indexNeg = replicate (numVols neg dwipairs) (show $ 1 +  numValidB0s pos dwipairs)

writeCombined :: FilePath -> [DWIPair] -> Action ()
writeCombined out dwipairs = do
  mergeVols out $ map (_dwi . pos) dwipairs ++ map (_dwi . neg) dwipairs
  trimVol out