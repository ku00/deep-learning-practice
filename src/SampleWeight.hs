module SampleWeight
    ( createBinarySW
    , loadSW
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ (compress, decompress)
import Data.Binary (encode, decode)
import Text.Parsec

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
    let basePath = generatePath p

    ws <- readFile $ basePath ++ ".csv"
    case parseCSV ws of
      Left e -> print e
      Right w -> do
        putStrLn $ "Creating binary Matrix file: " ++ basePath
        let wm = fromLists $ map (read :: String -> Double) <$> w
        createPickle (basePath ++ ".dat") wm
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
    encodeSW <- BL.readFile $ generatePath p ++ ".dat"
    return $ (decode . GZ.decompress) encodeSW

loadSW :: IO [Matrix R]
loadSW = forM weightFiles loadPickle
