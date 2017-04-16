import Numeric.LinearAlgebra
import ActivationFunction
import Mnist
import SampleWeight

batchSize = 100

predict :: SampleWeight -> Vector R -> Vector R
predict ([w1,w2,w3],[b1,b2,b3]) x =
    softMax' . (\x'' -> sumInput x'' w3 b3) . sigmoid . (\x' -> sumInput x' w2 b2) . sigmoid $ sumInput x w1 b1

sumInput :: Vector R -> Weight -> Bias -> Vector R
sumInput x w b = (x <# w) + b

maxIndexPredict :: SampleWeight -> Vector R -> Double
maxIndexPredict sw x = fromIntegral . maxIndex $ predict sw x

take' :: Indexable c t => Int -> Int -> c -> [t]
take' n1 n2 x
    | n1 >= n2  = []
    | otherwise = (x ! n1) : take' (n1+1) n2 x

increment :: [Double] -> [Double] -> Double
increment ps l = fromIntegral . length . filter id $ zipWith (==) ps l

countAccuracy :: Double -> Int -> SampleWeight -> DataSet -> Double
countAccuracy a n sw ds@(i,l)
    | n <= 0    = a
    | otherwise =
      if maxIndexPredict sw (i ! (n-1)) == l ! (n-1)
        then countAccuracy (a+1) (n-1) sw ds
        else countAccuracy a (n-1) sw ds

-- For batch
countAccuracy' :: Double -> Int -> SampleWeight -> DataSet -> Double
countAccuracy' a n sw ds@(i,l)
    | n <= 0    = a
    | otherwise = countAccuracy' (a+cnt) (n-batchSize) sw ds
        where ps = maxIndexPredict sw <$> take' (n-batchSize) n i
              ls = take' (n-batchSize) n l
              cnt = increment ps ls

main = do
    [_, ds] <- loadMnist True
    sw <- loadSW
    let r = rows $ fst ds
        cnt = countAccuracy' 0 r sw ds

    putStrLn $ "Accuracy: " ++ show (cnt / fromIntegral r)
