module Mnist
    ( initMnist
    ) where

import Numeric.LinearAlgebra
import Network.HTTP.Simple ( parseRequest, httpLBS, getResponseBody )
import System.Directory ( doesFileExist )
import Control.Monad ( unless )
import qualified Data.ByteString.Lazy as B ( writeFile )

type DataSet = (Matrix R, Matrix R)

urlBase = "http://yann.lecun.com/exdb/mnist"
keyFiles = [
            ("train_img", "train-images-idx3-ubyte.gz"),
            ("train_label", "train-labels-idx1-ubyte.gz"),
            ("test_img", "t10k-images-idx3-ubyte.gz"),
            ("test_label", "t10k-labels-idx1-ubyte.gz")
          ]

datasetDir = "assets"

download :: String -> IO ()
download fileName = do
    let filePath = datasetDir ++ "/" ++ fileName

    e <- doesFileExist filePath
    unless e $ do
      putStrLn $ "Downloading " ++ fileName ++ " ..."
      res <- httpLBS =<< parseRequest (urlBase ++ "/" ++ fileName)
      B.writeFile filePath (getResponseBody res)
      putStrLn "Done"

downloadMnist :: [(String, String)] -> IO ()
downloadMnist [] = return ()
downloadMnist (x:xs) = do
    download $ snd x
    downloadMnist xs

initMnist :: IO ()
initMnist = downloadMnist keyFiles
