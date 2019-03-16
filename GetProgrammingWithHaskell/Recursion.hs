gcd' :: Int -> Int -> Int
gcd' x y = if remainder == 0 then y else gcd' y remainder
  where remainder = x `mod` y

sayAmountCase :: Int -> String
sayAmountCase n = case n of
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

sayAmount :: Int -> String
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount _ = "a bunch"

tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail' []       = []

take' :: Int -> [a] -> [a]
take' _ []       = []
take' 0 _        = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []       = []
drop' 0 xs       = xs
drop' n (_ : xs) = drop' (n - 1) xs

length' :: [a] -> Int
length' []       = 0
length' (x : xs) = 1 + length' xs

cycle' :: [a] -> [a]
cycle' []       = []
cycle' (x : xs) = x : cycle' (xs ++ [x])

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz :: Int -> Int
collatz 1 = 1
collatz n | even n    = 1 + collatz (n `div` 2)
          | otherwise = 1 + collatz (n * 3 + 1)

reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = (reverse' xs) ++ [x]
