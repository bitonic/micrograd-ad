-- See <https://github.com/karpathy/micrograd/blob/c911406e5ace8742e5841a7e0df113ecb5d54685/micrograd/nn.py>
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MLP where

import qualified Data.Vector as V
import Data.List
import Data.Traversable
import Control.Monad
import qualified Numeric.AD.Mode.Reverse as AD
import qualified System.Random.MWC as MWC
import Data.Vector (Vector)

data Neuron a = Neuron
  { weights :: Vector a
  , bias :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

callNeuron :: (Num a) => Vector a -> (a -> a) -> Neuron a -> a
callNeuron xs activate neuron =
  activate (bias neuron + V.sum (V.zipWith (*) (weights neuron) xs))

newtype Layer a = Layer (Vector (Neuron a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

callLayer :: (Num a) => Vector a -> (a -> a) -> Layer a -> Vector a
callLayer inputs activation (Layer neurons) = V.map (callNeuron inputs activation) neurons

-- | We assume that the last layer always has one output.
newtype MLP a = MLP (Vector (Layer a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

reLU :: (Num a, Ord a) => a -> a
reLU x = if x > 0 then x else 0

-- | We don't apply reLU in the last layer since we'll do that
-- in the loss function. Also, we rely on the MLP having a scalar output.
callMLP :: (Num a, Ord a) => MLP a -> Vector a -> a
callMLP (MLP layers) inputs =
  V.head $ callLayer
    (foldl' (\xs -> callLayer xs reLU) inputs (V.init layers))
    id
    (V.last layers)

data Result a = Result
  { expectedOutput :: a
  , output :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

loss ::
    (Fractional a, Ord a)
 => MLP a
 -> Vector (Vector a, a)
 -- ^ training batch
 -> a
loss mlp samples = let
  mlpOutputs = V.map (callMLP mlp . fst) samples
  -- svm "max margin" loss
  losses = V.zipWith (\(_, expectedOutput) mlpOutput -> reLU (1 + (- expectedOutput) * mlpOutput)) samples mlpOutputs
  dataLoss = V.sum losses / fi (V.length losses)
  -- L2 regularization
  alpha = 1e-4
  regLoss = alpha * sum (fmap (\p -> p * p) mlp)
  in dataLoss + regLoss

optimizeStep ::
     (Floating a, Ord a)
  => MLP a -> Vector (Vector a, a) -> a -> MLP a
optimizeStep mlp batch learningRate =
  AD.gradWith
    (\x dx -> x - learningRate * dx)
    (\ad_mlp -> loss
        ad_mlp
        (V.map (\(inputs, output) -> (V.map AD.auto inputs, AD.auto output)) batch))
    mlp

-- | Given an initial model, and batches to train the model on,
-- iteratively trains the model returning each intermediate one.
optimize ::
     (Floating a, Ord a)
  => MLP a -> [Vector (Vector a, a)] -> [MLP a]
optimize mlp0 batches =
  scanl
    (\mlp (epoch, batch) ->
        optimizeStep mlp batch (1.0 - 0.9 * fi epoch / fi numEpochs))
    mlp0
    (zip [0 :: Int ..] batches)
  where
    numEpochs = length batches

initNeuron :: (MWC.Variate a, Num a) => MWC.GenIO -> Int -> IO (Neuron a)
initNeuron g inputs = do
  weights <- replicateM inputs (MWC.uniformR (-1, 1) g)
  return (Neuron (V.fromList weights) 0)

initLayer :: (Num a, MWC.Variate a) => MWC.GenIO -> Int -> Int -> IO (Layer a)
initLayer g inputs outputs = Layer <$> V.replicateM outputs (initNeuron g inputs)

-- | Initializes the MLP given the number of inputs of each layer.
-- The output is always a scalar.
initMLP :: (Num a, MWC.Variate a) => MWC.GenIO -> [Int] -> IO (MLP a)
initMLP g inputs =
  MLP . V.fromList <$> for (zip inputs (tail inputs ++ [1])) (uncurry (initLayer g))

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

