module Gradient
    (numericalDiff
    ) where

import Numeric.LinearAlgebra

numericalDiff :: Fractional a => (a -> a) -> a -> a
numericalDiff f x = (f (x+h) - f (x-h)) / (2*h)
    where h = 1e-4
