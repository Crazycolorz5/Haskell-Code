-- import Control.Arrow
import Control.Monad (liftM2)
import Data.Fixed (mod')

powerSeries :: (Int -> Double) -> Int -> (Double -> Double)
powerSeries f n x = foldl (\acc e -> acc + (liftM2 (*) f (x^)) e) 0 $ take n [0..]

sinCoef :: Int -> Double
sinCoef n = case mod n 4 of
                 0 -> 0
                 1 -> 1 / factorial n
                 2 -> 0
                 3 -> (-1) / factorial n
                 
derivativeCoef :: (Int -> Double) -> (Int -> Double)
derivativeCoef f n = f (n+1) * fromIntegral (n + 1)

integralCoef :: (Int -> Double) -> (Int -> Double)
integralCoef = undefined

factorial n = factorials !! n
factorials = zipWith (*) (1:[1..]) (1:factorials)

cosCoef = derivativeCoef sinCoef

lnOfPlusOneCoef n = if n == 0 then 0 else (-1)^(n-1) / (fromIntegral n) --From the integral of the geometric series with -r.
lnOfPlusOne = powerSeries lnOfPlusOneCoef 10000

transform x = (1-x)/(1+x) --domain = R+, range = (-1, 1)
myLn x = let y = transform x in lnOfPlusOne (-y) - lnOfPlusOne y --Thus |y| is in (-1, 1)

rem' x y = mod' x y + (if x < 0 then y else 0)

mySin x = powerSeries sinCoef 100 (rem' x (2*pi))
myCos x = powerSeries cosCoef 100 (rem' x (2*pi))

eCoef n = 1 / factorial n

myExp = powerSeries eCoef 100

arctanCoef n = if even n then 0 else (-1)^(div (n-1) 2)/(fromIntegral n)
myArctan = powerSeries arctanCoef 10000
