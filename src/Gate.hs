import Numeric.LinearAlgebra

and' x1 x2 =
    let w1 = 0.5; w2 = 0.5
        theta = 0.7
        tmp = x1*w1 + x2*w2
    in if tmp > theta then 1 else 0

and'' :: Num t => Vector R -> t
and'' x =
    let w = vector [0.5, 0.5]
        b = (-0.7)
        tmp = (x <.> w) + b
    in if tmp > 0 then 1 else 0

nand :: Num t => Vector R -> t
nand x =
    let w = vector [(-0.5), (-0.5)]
        b = 0.7
        tmp = (x <.> w) + b
    in if tmp > 0 then 1 else 0

or' :: Num t => Vector R -> t
or' x =
    let w = vector [0.5, 0.5]
        b = (-0.2)
        tmp = (x <.> w) + b
    in if tmp > 0 then 1 else 0
