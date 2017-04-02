module Mnist
    ( initMnist
    , loadImg
    , loadLabel
    ) where

import Numeric.LinearAlgebra
import Network.HTTP.Simple ( parseRequest, httpLBS, getResponseBody )
import System.Directory ( doesFileExist )
import Control.Monad ( unless, mapM_ )
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ ( decompress )

type DataSet = (Matrix R, Matrix R)

urlBase = "http://yann.lecun.com/exdb/mnist"
keyFiles = [
            ("train_img", "train-images-idx3-ubyte.gz"),
            ("train_label", "train-labels-idx1-ubyte.gz"),
            ("test_img", "t10k-images-idx3-ubyte.gz"),
            ("test_label", "t10k-labels-idx1-ubyte.gz")
          ]

dataSetDir = "assets"
imgSize = 784

generatePath :: String -> String
generatePath p = dataSetDir ++ "/" ++ p

download :: String -> IO ()
download fileName = do
    let savePath = generatePath fileName

    e <- doesFileExist savePath
    unless e $ do
      putStrLn $ "Downloading " ++ fileName ++ " ..."
      res <- httpLBS =<< parseRequest (urlBase ++ "/" ++ fileName)
      BL.writeFile savePath (getResponseBody res)
      putStrLn "Done"

downloadMnist :: [(String, String)] -> IO ()
downloadMnist [] = return ()
downloadMnist (x:xs) = do
    download $ snd x
    downloadMnist xs

toDoubleList :: BL.ByteString -> [Double]
toDoubleList = map (read . show . fromEnum) . BL.unpack

loadLabel :: String -> IO (Matrix R)
loadLabel fileName = do
    contents <- fmap GZ.decompress (BL.readFile $ generatePath fileName)
    return . matrix 1 . toDoubleList $ BL.drop 8 contents

loadImg :: String -> IO (Matrix R)
loadImg fileName = do
    contents <- fmap GZ.decompress (BL.readFile $ generatePath fileName)
    return . matrix imgSize . toDoubleList $ BL.drop 16 contents

initMnist :: IO ()
initMnist = downloadMnist keyFiles
