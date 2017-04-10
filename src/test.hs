import Mnist
import SampleWeight
import Numeric.LinearAlgebra

main = do
    sw <- loadSW
    print $ sw !! 3
    putStrLn "Done"
