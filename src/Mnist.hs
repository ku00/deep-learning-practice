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
download fn = do
    let p = generatePath fn

    e <- doesFileExist p
    unless e $ do
      putStrLn $ "Downloading " ++ fn ++ " ..."
      res <- httpLBS =<< parseRequest (baseUrl ++ "/" ++ fn)
      BL.writeFile p (getResponseBody res)
      putStrLn "Done"

downloadMnist :: [(String, String)] -> IO ()
downloadMnist [] = return ()
downloadMnist (x:xs) = do
    download $ snd x
    downloadMnist xs

toDoubleList :: BL.ByteString -> [Double]
toDoubleList = fmap (read . show . fromEnum) . BL.unpack

loadLabel :: String -> IO Label
loadLabel fn = do
    c <- fmap GZ.decompress (BL.readFile $ generatePath fn)
    return . vector . toDoubleList $ BL.drop 8 c

loadImg :: String -> IO Image
loadImg fn = do
    c <- fmap GZ.decompress (BL.readFile $ generatePath fn)
    return . matrix imgSize . toDoubleList $ BL.drop 16 c

convertDataset :: IO [DataSet]
convertDataset = do
    tri <- loadImg . snd . head $ keyFiles
    trl <- loadLabel . snd . (!!1) $ keyFiles
    ti <- loadImg . snd . (!!2) $ keyFiles
    tl <- loadLabel . snd . (!!3) $ keyFiles

    return [(tri, trl), (ti, tl)]

createPickle :: String -> [DataSet] -> IO ()
createPickle p ds = BL.writeFile p $ (GZ.compress . encode) ds

loadPickle :: String -> IO [DataSet]
loadPickle p = do
    eds <- BL.readFile p
    return $ (decode . GZ.decompress) eds

initMnist :: IO ()
initMnist = do
    downloadMnist keyFiles
    putStrLn "Creating binary DataSet file ..."
    createPickle (generatePath pickleFile) =<< convertDataset
    putStrLn "Done"

normalizeImg :: Bool -> [DataSet] -> IO [DataSet]
normalizeImg f ds@[tr, t]
    | f         = return [ ((/255) $ fst tr, snd tr), ((/255) $ fst t, snd t) ]
    | otherwise = return ds

loadMnist :: Bool -> IO [DataSet]
loadMnist nml = do
    let p = generatePath pickleFile
    e <- doesFileExist p
    unless e initMnist

    normalizeImg nml =<< loadPickle p
