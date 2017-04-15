module Mnist
    ( Image
    , Label
    , DataSet
    , loadMnist
    ) where

import Control.Monad
import Numeric.LinearAlgebra
import Network.HTTP.Simple (parseRequest, httpLBS, getResponseBody)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ (compress, decompress)
import Data.Binary (encode, decode)

type Image = Matrix R
type Label = Vector R
type DataSet = (Image, Label)

baseUrl = "http://yann.lecun.com/exdb/mnist"
keyFiles = [
            ("train_img", "train-images-idx3-ubyte.gz"),
            ("train_label", "train-labels-idx1-ubyte.gz"),
            ("test_img", "t10k-images-idx3-ubyte.gz"),
            ("test_label", "t10k-labels-idx1-ubyte.gz")
          ]

assetsDir = "assets"
pickleFile = "mnist.dat"
imgSize = 784

generatePath :: String -> String
generatePath p = assetsDir ++ "/" ++ p

download :: String -> IO ()
download fileName = do
    let savePath = generatePath fileName

    e <- doesFileExist savePath
    unless e $ do
      putStrLn $ "Downloading " ++ fileName ++ " ..."
      res <- httpLBS =<< parseRequest (baseUrl ++ "/" ++ fileName)
      BL.writeFile savePath (getResponseBody res)
      putStrLn "Done"

downloadMnist :: [(String, String)] -> IO ()
downloadMnist [] = return ()
downloadMnist (x:xs) = do
    download $ snd x
    downloadMnist xs

toDoubleList :: BL.ByteString -> [Double]
toDoubleList = fmap (read . show . fromEnum) . BL.unpack

loadLabel :: String -> IO Label
loadLabel fileName = do
    contents <- fmap GZ.decompress (BL.readFile $ generatePath fileName)
    return . vector . toDoubleList $ BL.drop 8 contents

loadImg :: String -> IO Image
loadImg fileName = do
    contents <- fmap GZ.decompress (BL.readFile $ generatePath fileName)
    return . matrix imgSize . toDoubleList $ BL.drop 16 contents

convertDataset :: IO [DataSet]
convertDataset = do
    trainImg <- loadImg . snd . head $ keyFiles
    trainLabel <- loadLabel . snd . (!!1) $ keyFiles
    testImg <- loadImg . snd . (!!2) $ keyFiles
    testLabel <- loadLabel . snd . (!!3) $ keyFiles

    return [(trainImg, trainLabel), (testImg, testLabel)]

createPickle :: String -> [DataSet] -> IO ()
createPickle p ds = BL.writeFile p $ (GZ.compress . encode) ds

loadPickle :: String -> IO [DataSet]
loadPickle p = do
    encodeDs <- BL.readFile p
    return $ (decode . GZ.decompress) encodeDs

initMnist :: IO ()
initMnist = do
    downloadMnist keyFiles
    putStrLn "Creating binary DataSet file ..."
    createPickle (generatePath pickleFile) =<< convertDataset
    putStrLn "Done"

normalizeImg :: Bool -> [DataSet] -> IO [DataSet]
normalizeImg f ds@[train, test]
    | f         = return [ ((/255) $ fst train, snd train), ((/255) $ fst test, snd test) ]
    | otherwise = return ds

loadMnist :: Bool -> IO [DataSet]
loadMnist normalize = do
    let loadPath = generatePath pickleFile
    e <- doesFileExist loadPath
    unless e initMnist

    loadPickle loadPath >>= normalizeImg normalize
