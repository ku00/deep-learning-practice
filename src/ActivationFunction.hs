module ActivationFunction
    ( step'
    , sigmoid
    , relu
    , softMax'
    ) where

import Numeric.LinearAlgebra

step' :: (Element b, Container c a, Ord a, Num a, Num b) => c a -> c b
step' = cmap (\x -> if x>0 then 1 else 0)

sigmoid :: (Container c b, Floating b) => c b -> c b
sigmoid = cmap (\x -> 1 / (1 + exp(-x)))

relu :: (Container c b, Ord b, Num b) => c b -> c b
relu = cmap (max 0)

-- This function is deprecated, causes overflow.
softMax :: (Container c b, Floating b) => c b -> c b
softMax xs = cmap (/sumCexp) $ cexp xs
    where sumCexp = (sumElements . cexp) xs

softMax' :: (Container c b, Floating b) => c b -> c b
softMax' xs = cmap (/sumCexp) $ cexp' xs
    where sumCexp = (sumElements . cexp') xs

cexp :: (Container c b, Floating b) => c b -> c b
cexp = cmap exp

cexp' :: (Container c b, Floating b) => c b -> c b
cexp' xs = cexp $ cmap (subtract $ maxElement xs) xs
