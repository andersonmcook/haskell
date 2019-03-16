import Data.Char (isDigit)

eitherHead :: [a] -> Either String a
eitherHead []      = Left "There is no head because the list is empty"
eitherHead (x : _) = Right x

{-
The Either type is also a member of Monad (and thus Functor and Applicative)
-}

eitherIncrementRight :: Either String Int
eitherIncrementRight = (+ 1) <$> (eitherHead [1, 2, 3]) -- Right 2

eitherIncrementLeft :: Either String Int
eitherIncrementLeft = (+ 1) <$> (eitherHead []) -- Left "error message"

primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2     = Left InvalidValue
          | n > maxN  = Left TooLarge
          | otherwise = Right $ n `elem` primes

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True      ) = "It's prime"
displayResult (Right False     ) = "It's composite"
displayResult (Left  primeError) = show primeError

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  print $ displayResult $ isPrime n

-- Q38.1
allDigits :: String -> Bool
allDigits a = all (== True) . map isDigit

-- doesn't work, but i'm tired
addStrInts :: String -> String -> Either String Int
addStrInts m n
  | all allDigits [m, n]         = Right $ sum $ map read [m, n]
  | all (not . allDigits) [m, n] = Left "Neither value can be parsed"
  | not . allDigits m            = Left "First value can't be parsed"
  | otherwise                    = Left "Second value can't be parsed"
