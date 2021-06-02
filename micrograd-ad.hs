-- See <https://github.com/karpathy/micrograd/blob/c911406e5ace8742e5841a7e0df113ecb5d54685/demo.ipynb>
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Traversable
import Control.Monad
import Data.Foldable
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import Text.Printf (printf)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import Data.Vector (Vector)

import MLP

initMoonClassifier :: MWC.GenIO -> IO (MLP Double)
initMoonClassifier gen = initMLP gen [2, 16, 16]

optimizeMoonClassifier :: MWC.GenIO -> IO [MLP Double]
optimizeMoonClassifier gen = do
  let epochs = 150
  let batchSize = 50
  optimize
    <$> initMoonClassifier gen
    <*> replicateM epochs (V.map moonDataPoint <$> makeMoons gen batchSize)

main :: IO ()
main = do
  gen <- MWC.initialize (V.singleton 42)
  mlps <- optimizeMoonClassifier gen
  testMoonPoints <- makeMoons gen 1000
  createDirectoryIfMissing False "output"
  T.writeFile (printf "output/test-moons.cv") (moonsCsv testMoonPoints)
  let testMoons = V.map moonDataPoint testMoonPoints
  for_ (zip [0 :: Int ..] mlps) $ \(epoch, mlp) -> do
    let mlpResults = V.map (callMLP mlp . fst) testMoons
    let correct :: Int =
          V.sum (V.zipWith (\(_, expectedOutput) mlpOutput -> if signum expectedOutput == signum mlpOutput then 1 else 0) testMoons mlpResults)
    let accuracy :: Double = 100 * fi correct / fi (V.length testMoons)
    let l = loss mlp testMoons
    putStrLn (printf "Epoch %03d -- loss: %0.2f, accuracy: %0.2f%%" epoch l accuracy)
    T.writeFile (printf "output/%03d-contour.csv" epoch) (mlpCsv testMoonPoints mlp)
    T.writeFile (printf "output/%03d-loss" epoch) (T.pack (show l))

data WhichMoon = Upper | Lower
  deriving (Eq, Show)

whichMoonToNum :: (Num a) => WhichMoon -> a
whichMoonToNum = \case
  Upper -> -1
  Lower ->  1

-- | Position and whether is in the upper or lower semicircle
type MoonPoint = ((Double, Double), WhichMoon)

moonDataPoint :: MoonPoint -> (Vector Double, Double)
moonDataPoint ((x, y), which) = (V.fromList [x, y], whichMoonToNum which)

shuffle :: MWC.GenIO -> Vector a -> IO (Vector a)
shuffle gen v = do
  vm <- V.thaw v
  for_ [0 .. V.length v - 2] $ \fromIx -> do
    toIx <- MWC.uniformR (fromIx, V.length v - 1) gen
    VM.swap vm fromIx toIx
  V.freeze vm

-- See <https://github.com/scikit-learn/scikit-learn/blob/95119c13af77c76e150b753485c662b7c52a41a2/sklearn/datasets/_samples_generator.py#L684>
makeMoons :: MWC.GenIO -> Int -> IO (Vector MoonPoint)
makeMoons gen numSamples = do
  let upperCirc = do
        n <- [0 .. numSamplesUpper - 1]
        let x = 0 + (pi / fi numSamplesUpper) * fi n
        return (cos x, sin x)
  let lowerCirc = do
        n <- [0 .. numSamplesLower - 1]
        let x = 0 + (pi / fi numSamplesLower) * fi n
        return (1 - cos x, 1 - sin x - 0.5)
  -- shuffle
  circ <- shuffle gen (V.fromList (zip upperCirc (repeat Upper) ++ zip lowerCirc (repeat Lower)))
  -- add noise
  for circ $ \((x, y), which) -> do
    noise_x <- MWC.normal 0 0.1 gen
    noise_y <- MWC.normal 0 0.1 gen
    return ((x + noise_x, y + noise_y), which)
  where
    numSamplesUpper = numSamples `div` 2
    numSamplesLower = numSamples - numSamplesUpper

moonsCsv :: Vector MoonPoint -> T.Text
moonsCsv moons = T.unlines $ do
  ((x, y), which) <- V.toList moons
  return (T.intercalate "," (map T.pack [show x, show y, show (whichMoonToNum which :: Int)]))

mlpCsv :: Vector MoonPoint -> MLP Double -> T.Text
mlpCsv testMoons mlp = T.unlines $ do
  x <- [min_x, min_x + h .. max_x]
  y <- [min_y, min_y + h .. max_y]
  return (T.intercalate "," (map T.pack [show x, show y, show (callMLP mlp (V.fromList [x, y]))]))
  where
    min_x = V.minimum (V.map (\((x, _), _) -> x) testMoons) - 0.25
    max_x = V.maximum (V.map (\((x, _), _) -> x) testMoons) + 0.25
    min_y = V.minimum (V.map (\((_, y), _) -> y) testMoons) - 0.25
    max_y = V.maximum (V.map (\((_, y), _) -> y) testMoons) + 0.25
    h = 0.10
