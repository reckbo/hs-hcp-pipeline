{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Csv
import           Data.Function
import           Data.List
import           Development.Shake
import           FSL
import           GHC.Generics

pas :: [FilePath]
pas = [
  "BIO_0001.dwiPA1.nii.gz",
  "BIO_0001.dwiPA2.nii.gz"
  ]

aps :: [FilePath]
aps = [
  "BIO_0001.dwiAP1.nii.gz",
  "BIO_0001.dwiAP2.nii.gz"
  ]

data PhaseDirection = RL | PA

-- phaseDirection = PA

b0maxbval :: BValue
b0maxbval = BValue 50

b0dist :: Int
b0dist = 45

-- phaseLength :: FilePath -> Action PhaseLength
-- phaseLength dwi = case phaseDirection of
--   PA -> read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
--   _   -> read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

type EchoSpacing = Float
type PhaseLength = Int

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

type DWI = FilePath

acqParamsPos :: String
acqParamsPos = "0 1 0 0.8"
acqParamsNeg = "0 -1 0 0.8"

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
     pairId               :: Int
    ,dwi                  :: FilePath
    ,size                 :: Int
    ,b0indices            :: [Int]
    ,b0indicesWithMinDist :: [Int]
    ,b0indicesToUse       :: [Int]
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
    writeB0 dwiinfo = extractVols (dwi dwiinfo) (b0indicesToUse dwiinfo)

numValidB0s :: (DWIPair -> DWIInfo) -> [DWIPair] -> Int
numValidB0s posneg xs = sum $  map (length . b0indicesToUse . posneg) xs

writeAcqparms :: FilePath -> [DWIPair] -> Action ()
writeAcqparms out dwipairs =
  writeFile' out $ unlines (acq ++ acq')
  where
    acq = replicate (numValidB0s pos dwipairs) acqParamsPos
    acq' = replicate (numValidB0s neg dwipairs) acqParamsNeg

writeIndex :: FilePath -> [DWIPair] -> Action ()
writeIndex out dwipairs = writeFile' out (unlines $ indexPos ++ indexNeg)
  where
    numVols posneg = sum . map (size . posneg)
    indexPos = replicate (numVols pos dwipairs) "0"
    indexNeg = replicate (numVols neg dwipairs) (show $ 1 +  numValidB0s pos dwipairs)

writeCombined :: FilePath -> [DWIPair] -> Action ()
writeCombined out dwipairs
  = mergeVols out $ map (dwi . pos) dwipairs ++ map (dwi . neg) dwipairs

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    want ["build/topup/PosNeg.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    [ "build/topup/PosNeg.nii.gz",
      "build/topup/B0s.nii.gz",
      "build/topup/acqparams.txt",
      "build/topup/index.txt",
      "build/topup/summary.csv"] *>>
      \[outvol, outb0s, acqparams, outindex, summaryCsv] -> do
      need $ pas ++ aps
      let
        readDWIPair (pid, dwi, dwi') =
          mkDWIPair <$>
            pure pid <*>
            pure dwi <*>
            pure dwi' <*>
            readbval (tobval dwi) <*>
            readbval (tobval dwi')
        toRecords (DWIPair a b') = [a,b']
      dwiPairs <- traverse readDWIPair $ zip3 [1..] pas aps
      writeFile' summaryCsv $ B.unpack $ encodeDefaultOrderedByName (concatMap toRecords dwiPairs)
      writeB0s outb0s $ (map pos dwiPairs) ++ map neg dwiPairs
      writeCombined outvol dwiPairs
      writeIndex outindex dwiPairs
      writeAcqparms acqparams dwiPairs
