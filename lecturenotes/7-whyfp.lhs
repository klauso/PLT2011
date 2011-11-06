This is Haskell code corresponding to Sec. 4 of Hughes "Why Functional Programming Matters."
Read this section in the paper in parallel with this code.

> next :: Double -> Double -> Double
> next n x = (x + n/x) / 2

In the paper this function is called "repeat". Since this name is predefined
in the Haskell prelude, and as a tribute to Casablanca, we call it playitagainsam.

> playitagainsam :: (a -> a) -> a -> [a]
> playitagainsam f a = a : playitagainsam f (f a)

> within :: Double -> [Double] -> Double
> within eps (a:b:rest) | abs (a-b) <= eps = b
> within eps (a:b:rest) = within eps(b:rest)

> sqrtabs :: Double -> Double -> Double -> Double
> sqrtabs a0 eps n = within eps (playitagainsam (next n) a0)

> relative :: Double -> [Double] -> Double
> relative eps (a:b:rest) | abs (a-b) <= eps*(abs b) = b
> relative eps (a:b:rest) = relative eps (b:rest)

> sqrtrel :: Double -> Double -> Double -> Double
> sqrtrel a0 eps n = relative eps (playitagainsam (next n) a0)