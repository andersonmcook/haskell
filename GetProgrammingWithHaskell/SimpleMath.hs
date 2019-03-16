inc :: Num a => a -> a
inc = (+) 1

double :: Num a => a -> a
double = (*) 2

square :: Num a => a -> a
square n = n * n

f :: Int -> Int
f n = if even n then n - 2 else 3 * n + 1


ifEven :: (Int -> Int) -> Int -> Int
ifEven f x = if even x then f x else x

genIfEven :: (Int -> Int) -> (Int -> Int)
genIfEven f = \x -> ifEven f x

ifEvenInc :: Int -> Int
ifEvenInc = genIfEven inc
