{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import FSL
import Data.List
import           Development.Shake
import Data.Csv

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

b0maxbval :: Int
b0maxbval = 45

-- phaseLength :: FilePath -> Action PhaseLength
-- phaseLength dwi = case phaseDirection of
--   PA -> read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
--   _   -> read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

type EchoSpacing = Float
type PhaseLength = Int

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

type BVal = FilePath
type DWI = FilePath
type Bvalue = Int

acqParamsPos :: String
acqParamsPos = "0 1 0 0.8"
acqParamsNeg = "0 -1 0 0.8"

getB0Indices :: [Int] -> [Int]
getB0Indices = findIndices (< b0maxbval)

mkDWIPair :: DWI -> DWI -> [Bvalue] -> [Bvalue] -> DWIPair
mkDWIPair v v' bs bs'  = DWIPair v v' bs bs' b0indices b0indices'
  where
    paired = zip bs bs'
    b0indices = getB0Indices . map fst $ paired
    b0indices' = getB0Indices . map snd $ paired
    -- p = replicate (length b0indices) acqParamsPos
    -- p' = replicate (length b0indices') acqParamsNeg

data DWIPair = DWIPair
  { pDwi        :: FilePath
  , pDwi'       :: FilePath
  , b0s        :: [Int]
  , b0s'       :: [Int]
  , idx        :: [Int]
  , idx'       :: [Int]
  }

data DWIPairInfo = DWIPairInfo
  { posDWI :: String,
    negDWI :: String,
    numPosB0s :: Int,
    numNegB0s :: Int,
    posB0indices :: [Int],
    negB0indices :: [Int],
    posB0indicesUsed :: [Int],
    negB0indicesUsed :: [Int]
  }

instance ToNamedRecord DWIPairInfo
instance DefaultOrdered DWIPairInfo

info :: DWIPair -> DWIPairInfo
info (DWIPair {..}) = DWIPairInfo pDwi pDwi' (length b0s) (length b0s') (getB0Indices b0s) (getB0Indices b0s') idx idx'

writePosB0s :: FilePath -> [DWIPair] -> Action ()
writePosB0s out dwipairs =
  do fs <- traverse writePosB0 dwipairs
     mergeVols out fs
  where writePosB0 dwipair = extractVols (pDwi dwipair) (idx dwipair)

writeNegB0s :: FilePath -> [DWIPair] -> Action ()
writeNegB0s out dwipairs =
  do fs <- traverse writeNegB0 dwipairs
     mergeVols out fs
  where writeNegB0 dwipair = extractVols (pDwi' dwipair) (idx' dwipair)

writeAcqparms :: FilePath -> [DWIPair] -> Action ()
writeAcqparms out dwipairs =
  writeFile' out $ unlines (acq ++ acq')
  where
    numPosB0s = sum $ map (length . idx) dwipairs
    numNegB0s = sum $ map (length . idx') dwipairs
    acq = replicate numPosB0s acqParamsPos
    acq' = replicate numNegB0s acqParamsNeg

writeB0s :: FilePath -> [DWIPair] -> Action ()
writeB0s out dwipairs = do
  writePosB0s "Pos_B0.nii.gz" dwipairs
  writeNegB0s "Neg_B0.nii.gz" dwipairs
  mergeVols out ["Pos_B0.nii.gz", "Neg_B0.nii.gz"]

writeIndex :: FilePath -> [DWIPair] -> Action ()
writeIndex out dwipairs = writeFile' out (unlines $ indexPos ++ indexNeg)
  where
    numPos = length $ concatMap b0s dwipairs
    numNeg = length $ concatMap b0s' dwipairs
    numPosB0s = sum $ map (length . idx) dwipairs
    indexPos = replicate numPos "0"
    indexNeg = replicate  numNeg (show numPosB0s)

writeCombined :: FilePath -> [DWIPair] -> Action ()
writeCombined out dwipairs
  = mergeVols out $ (map pDwi dwipairs) ++ (map pDwi' dwipairs)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    want ["build/topup/Pos_Neg_b0.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    [ "build/topup/Pos_Neg.nii.gz",
      "build/topup/Pos_Neg_b0.nii.gz",
      "build/topup/acqparams.txt",
      "build/topup/index.txt",
      "build/topup/summary.csv"] *>> \[outvol, outb0s, acqparams, index, summary] -> do
      need $ pas ++ aps
      let
        readDWIPair (dwi, dwi') =
          mkDWIPair <$>
            (pure dwi) <*>
            (pure dwi') <*>
            (readbval $ tobval dwi) <*>
            (readbval $ tobval dwi')
      dwiPairs <- traverse readDWIPair $ zip pas aps
      writeFile' summary $ encodeDefaultOrderedByName (map info dwiPairs)
      writeB0s outb0s dwiPairs
      writeCombined outvol dwiPairs
      writeIndex index dwiPairs
      writeAcqparms acqparams dwiPairs
