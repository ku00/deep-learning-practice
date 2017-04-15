import Control.Monad
import Numeric.LinearAlgebra
import ActivationFunction
import Mnist
import SampleWeight

predict :: ([Matrix R], [Vector R]) -> Vector R -> Vector R
predict ([w1,w2,w3],[b1,b2,b3]) x =
    softMax' . (\x'' -> sumInput x'' w3 b3) . sigmoid . (\x' -> sumInput x' w2 b2) . sigmoid $ sumInput x w1 b1

sumInput :: Vector R -> Matrix R -> Vector R -> Vector R
sumInput x w b = (x <# w) + b

maxIndexPredict :: ([Matrix R], [Vector R]) -> Matrix R -> Int -> Double
maxIndexPredict sw i n = fromIntegral . maxIndex $ predict sw (i ! n)

countAccuracy a n sw i l
    | n <= 0    = a
    | otherwise =
      if maxIndexPredict sw i (n-1) == l ! (n-1)
        then countAccuracy (a+1) (n-1) sw i l
        else countAccuracy a (n-1) sw i l

main = do
    [_, (img, label)] <- loadMnist True
    sw <- loadSW
    let cnt = countAccuracy 0 (rows img) sw img label

    putStrLn $ "Accuracy: " ++ show (cnt / fromIntegral (rows img))
