module Mnist
    ( initMnist
    , loadImg
    ) where

import Numeric.LinearAlgebra
import Network.HTTP.Simple ( parseRequest, httpLBS, getResponseBody )
import System.Directory ( doesFileExist )
import Control.Monad ( unless, mapM_ )
import qualified Data.ByteString.Lazy as BL (unpack, drop, writeFile, readFile)
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

loadImg :: String -> IO (Matrix R)
loadImg fileName = do
    let loadPath = generatePath fileName

    putStrLn $ "Converting " ++ fileName ++ " to Matrix ..."
    contents <- fmap GZ.decompress (BL.readFile loadPath)
    putStrLn "Done"

    let unpackData = map (read . show . fromEnum) $ BL.unpack $ BL.drop 16 contents
    return $ matrix imgSize unpackData

initMnist :: IO ()
initMnist = downloadMnist keyFiles
