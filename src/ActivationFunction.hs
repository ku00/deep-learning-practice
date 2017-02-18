module ActivationFunction
    ( step'
    , sigmoid
    , relu
    ) where

import Numeric.LinearAlgebra

step' :: Vector R -> Vector R
step' = cmap (\x -> if x>0 then 1 else 0)

sigmoid :: Vector R -> Vector R
sigmoid = cmap (\x -> 1 / (1 + exp(-x)))

relu :: Vector R -> Vector R
relu = cmap (max 0)
