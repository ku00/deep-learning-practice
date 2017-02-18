module ActivationFunction
    ( step'
    ) where

import Numeric.LinearAlgebra

step' :: Vector R -> Vector R
step' = cmap (\x -> if x>0 then 1 else 0)
