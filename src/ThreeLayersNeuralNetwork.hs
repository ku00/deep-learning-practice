import ActivationFunction
import Numeric.LinearAlgebra

main = do
    let x = (1><2) [1.0, 0.5] :: Matrix R
        y = forward x
    print y

forward x =
    let w1 = (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6] :: Matrix R
        w2 = (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6] :: Matrix R
        w3 = (2><2) [0.1, 0.3, 0.2, 0.4] :: Matrix R
        b1 = (1><3) [0.1, 0.2, 0.3] :: Matrix R
        b2 = (1><2) [0.1, 0.2] :: Matrix R
        b3 = (1><2) [0.1, 0,2] :: Matrix R
        a1 = (x <> w1) + b1
        z1 = sigmoid a1
        a2 = (z1 <> w2) + b2
        z2 = sigmoid a2
        a3 = (z2 <> w3) + b3
    in id a3
