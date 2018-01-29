module Arith3Broken where


munge :: (x -> y)
    -> (y -> (w, z))
    -> x
    -> w
munge xy ywz x = w
    where
        y = xy x
        (w, z) = ywz y