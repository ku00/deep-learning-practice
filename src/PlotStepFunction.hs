import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

import ActivationFunction
import Numeric.LinearAlgebra.Data

main = toFile def "step_function.svg" $ do
    layout_title .= "a Graph of step function"
    plot (line "" [signals $ vector [-5..5]])

signals xs = zipWith (,) (toList xs) (toList $ step' xs)
