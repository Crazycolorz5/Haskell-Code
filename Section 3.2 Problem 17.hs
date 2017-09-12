{-
[2 of 2] Compiling Main             ( D:\Crazycolorz5\Dropbox\My Code\Haskell Code\QuadraticResidues.hs, interpreted )
Ok, 2 modules loaded.
*Main> x
True
-}
x = and $ do
    a <- [0..48]
    b <- [0..48]
    return (not (mod (19*a*a) 7 == mod (b*b) 7) || (mod (19*a*a) 49 == mod (b*b) 49))
    --Logically equivalent to 19a^2 = b^2 (mod 7) => 19a^2 = b^2 (mod 49)
