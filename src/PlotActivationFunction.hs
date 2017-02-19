import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

import ActivationFunction
import Numeric.LinearAlgebra.Data(vector, toList)

main = toFile def "activation_function.svg" $ do
    layout_title .= "activation function"
    let input = vector [(-5),(-4.9)..5]
    plot (line "step" [stepSignals input])
    plot (line "sigmoid" [sigmoidSignals input])
    plot (line "relu" [reluSignals input])

stepSignals xs = zipWith (,) (toList xs) (toList $ step' xs)

sigmoidSignals xs = zipWith (,) (toList xs) (toList $ sigmoid xs)

reluSignals xs = zipWith (,) (toList xs) (toList $ relu xs)
