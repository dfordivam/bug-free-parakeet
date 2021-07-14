#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ aeson lens bytestring containers time ])"

{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import Data.Time
import Data.Time.Format
import Control.Lens
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import System.Environment

main = do
  putStrLn "Usage: ./main.hs ref_file new_file out_file"
  (refFile: newFile: outFile: _) <- getArgs
  refData <- parseFile refFile
  newData <- parseFile newFile
  let finalDataValues = makeVals refData newData

  BSL.writeFile outFile (encode finalDataValues)

-- (136,"06:15:39.1447","06:15:39.1508","06:15:47.2615","06:15:47.4180","Distribution.Types.AbiDependency")
type InpRow = (Int, String, String, String, String, String)

type ParsedVal = (String, (TimeOfDay, TimeOfDay), (TimeOfDay, TimeOfDay))
type ParsedVals = Map.Map Int ParsedVal

parseFile :: String -> IO ParsedVals
parseFile inpF = do
  inpData <- readFile inpF
  let 
    f :: InpRow -> (Int, ParsedVal)
    f (i, tc1, tc2, bk1, bk2, mod) =
      (i, (mod, (parseT tc1, parseT tc2), (parseT bk1, parseT bk2)))
    inpRows :: [InpRow]
    inpRows = map read ( lines inpData)
    parseT :: String -> TimeOfDay
    parseT = parseTimeOrError False defaultTimeLocale "%H:%M:%S%Q"
  pure (Map.fromList (map f inpRows))


data DataValue = DataValue
  { mod :: String
  , step :: String
  , start :: DiffTime
  , end :: DiffTime
  }
  deriving (Generic, Show)

makeVals :: ParsedVals -> ParsedVals -> [DataValue]
makeVals ref new = concat (map g (Map.toList ref))
  where
    (_, (refStartTime, _), _) = ref Map.! 1
    (_, (newStartTime, _), _) = new Map.! 1
    g :: (Int , ParsedVal) -> [DataValue]
    g (i, (mod, (rTcS, rTcE), (rBkS, rBkE))) =
      [ DataValue mref "tc" (refT rTcS) (refT rTcE)
      , DataValue mref "bak" (refT rBkS) (refT rBkE)
      , DataValue mnew "tc2" (newT nTcS) (newT nTcE)
      , DataValue mnew "bak2" (newT nBkS) (newT nBkE)
      ]
      where
        refT t = max 0 ((timeOfDayToTime t) - (timeOfDayToTime refStartTime))
        newT t = max 0 ((timeOfDayToTime t) - (timeOfDayToTime newStartTime))
        -- modN = mod ++ "[" ++ show i ++ "]"
        modN = show (1000 + i)
        mref = modN ++ "(A)"
        mnew = modN ++ "(B)"
        (_, (nTcS, nTcE), (nBkS, nBkE)) = new Map.! i

instance ToJSON DataValue where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DataValue
