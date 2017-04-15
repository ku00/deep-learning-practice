module SampleWeight
    ( Weight
    , Bias
    , SampleWeight
    , createBinarySW
    , loadSW
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ (compress, decompress)
import Data.Binary (encode, decode)
import Text.Parsec

type Weight = Matrix R
type Bias = Vector R
type SampleWeight = ([Weight], [Bias])

assetsDir = "assets"
weightFiles = [ "sample-weight-w1"
              , "sample-weight-w2"
              , "sample-weight-w3"
              , "sample-weight-b1"
              , "sample-weight-b2"
              , "sample-weight-b3"
              ]

generatePath :: String -> String
generatePath p = assetsDir ++ "/" ++ p

createBinary :: String -> IO ()
createBinary p = do
    let bp = generatePath p

    ws <- readFile $ bp ++ ".csv"
    case parseCSV ws of
      Left e -> print e
      Right w -> do
        putStrLn $ "Creating binary Matrix file: " ++ bp
        let wm = fromLists $ fmap (read :: String -> Double) <$> w
        createPickle (bp ++ ".dat") wm
        putStrLn "Done"

createBinarySW :: IO ()
createBinarySW = forM_ weightFiles createBinary

csvStruct = endBy line eol
line = sepBy cell $ char ','
cell = many $ noneOf ",\n"
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvStruct "* ParseError *"

createPickle :: String -> Matrix R -> IO ()
createPickle p w = BL.writeFile p $ (GZ.compress . encode) w

loadPickle :: String -> IO (Matrix R)
loadPickle p = do
    esw <- BL.readFile $ generatePath p ++ ".dat"
    return $ (decode . GZ.decompress) esw

loadSW :: IO SampleWeight
loadSW = do
    sw <- forM weightFiles loadPickle
    let (w,b) = splitAt 3 sw
    return (w, fmap flatten b)
