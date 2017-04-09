import Mnist
import Numeric.LinearAlgebra

main = do
    ds <- loadMnist False
    print $ takeRows 1 $ fst $ head ds
