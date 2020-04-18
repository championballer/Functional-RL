{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Control.Monad (when)
import GHC.Generics
import System.Random (mkStdGen, randoms)
import Prelude hiding (exp)

import Torch
import qualified Torch.Typed.Vision as V hiding (getImages')
import qualified Torch.Vision as V
import Torch.Serialize

data MLPSpec = MLPSpec {
    inputFeatures :: Int,
    hiddenFeatures0 :: Int,
    hiddenFeatures1 :: Int,
    outputFeatures :: Int
    } deriving (Show, Eq)

data MLP = MLP { 
    l0 :: Linear,
    l1 :: Linear,
    l2 :: Linear
    } deriving (Generic, Show)

instance Parameterized MLP

instance Randomizable MLPSpec MLP where
    sample MLPSpec {..} = MLP 
        <$> sample (LinearSpec inputFeatures hiddenFeatures0)
        <*> sample (LinearSpec hiddenFeatures0 hiddenFeatures1)
        <*> sample (LinearSpec hiddenFeatures1 outputFeatures)

randomIndexes :: Int -> [Int]
randomIndexes size = (`mod` size) <$> randoms seed where seed = mkStdGen 123

mlp :: MLP -> Tensor -> Tensor
mlp MLP{..} input = 
    logSoftmax (Dim 1)
    . linear l2
    . relu
    . linear l1
    . relu
    . linear l0
    $ input

-- train 

