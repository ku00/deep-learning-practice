module LossFunction
    ( meanSquaredError
    , crossEntropyError
    ) where

import Numeric.LinearAlgebra

meanSquaredError :: (Container c b, Num (c b), Floating b) => c b -> c b -> b
meanSquaredError x y = (*0.5) . sumElements $ (x-y)^2

crossEntropyError :: (Container c b, Num (c b), Floating b) => c b -> c b -> b
crossEntropyError x y = (*(-1)) . sumElements $ y * cmap (\x -> log(x+1e-7)) x
