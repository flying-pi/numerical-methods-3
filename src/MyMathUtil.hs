module MyMathUtil where

roundFloat:: Double -> Integer -> Double
roundFloat f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)