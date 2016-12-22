module Methods where

data VariantDouble = Result Double | Unknown

halfMethod :: (Double -> Double) -> Double -> Double -> Double -> Double
halfMethod function a b e
    | e > (abs (a-b)) = a
    | function a == 0 = a
    | function b == 0 = b
    | function ((a+b)/2) == 0 = (a+b)/2
    | otherwise = if (((function a) * (function c)) < 0)
        then halfMethod function a c e
        else halfMethod function c b e
            where c = (a+b)/2

tangentsMethod :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> VariantDouble
tangentsMethod fun dfun ddfun a b e
    |(((dfun a)*(ddfun a))) >0 =  Result (tangentsMethod1 fun dfun a e)
    |(((dfun b)*(ddfun b))) >0 =  Result (tangentsMethod1 fun dfun b e)
    | otherwise = Unknown


tangentsMethod1 :: (Double -> Double)->(Double -> Double) -> Double ->  Double -> Double
tangentsMethod1 fun dfun xn e
    | (abs((fun xn)/(dfun xn))) < e = xn
    | otherwise = tangentsMethod1 fun dfun (xn - ((fun xn)/(dfun xn))) e


hordMethod :: (Double -> Double) -> Double ->  Double ->  Double -> Double
hordMethod f a b e
    | e > (abs (a-b)) = b
    | otherwise = hordMethod f newA newB e where
        newA =  b - (b - a) * (f b)/((f b) - (f a ))
        newB = newA + (newA - b) * (f newA)/((f newA) - (f b))

kombinationMetod :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> Double
kombinationMetod fun dfun ddfun a b e
    | 2*e > (abs (a-b)) = (a +b )/2
    | otherwise =kombinationMetod fun dfun ddfun newA newB e where
      newA = if (((fun a)*(ddfun a))<0) then
            a - (fun a) *((a-b)/((fun a) - (fun b)))
        else
            a - ((fun a)/(dfun b))
      newB = if (((fun a)*(ddfun a))<0) then
            b - (fun b)*((b-newA)/((fun b)-(fun newA)))
        else
            b - (fun b)/(dfun b)


approximationMethod  :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
approximationMethod fun dfun x e = if (e > (abs (x-newX))) then newX  else  approximationMethod fun dfun newX e
    where newX = x - (fun x)/(dfun x)