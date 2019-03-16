import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2 ^ value)

powersOfTwoComp :: Int -> [Int]
powersOfTwoComp n = [ 2 ^ value | value <- [1 .. n] ]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powersOfTwo   = 2 ^ value
  let powersOfThree = 3 ^ value
  return (powersOfTwo, powersOfThree)

powersOfTwoAndThreeComp :: Int -> [(Int, Int)]
powersOfTwoAndThreeComp n = [ (2 ^ value, 3 ^ value) | value <- [1 .. n] ]
-- or
{-
powersOfTwoAndThreeComp2 n =
  [ (powerOfTwo, powerOfThree)
  | value <- [1 .. n]
  , let powerOfTwo   = 2 ^ value
  , let powerOfThree = 3 ^ value
  ]
-}

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue  <- [1, 3 .. n]
  return (evenValue, oddValue)

allEvenOddsComp :: Int -> [(Int, Int)]
allEvenOddsComp n = [ (x, y) | x <- [2, 4 .. n], y <- [1, 3 .. n] ]

valAndSquareUpToTen :: [(Int, Int)]
valAndSquareUpToTen = do
  val <- [1 .. 10]
  return (val, val ^ 2)

{-
Control.Monad

guard :: Alternative f => Bool - f()

Alternative is a subclass of Applicative (all Alternatives are Applicatives)

not all Monads are instances of Alternative

Alternative implements a function called empty.  It works like mempty for Monoids

List and Maybe are instances of Alternative. empty is [] for List and Nothing for Maybe

IO is not an instance of Alternative.  You can't use guard with IO
-}

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard $ even value
  return value

evensGuardComp :: Int -> [Int]
evensGuardComp n = [ x | x <- [1 .. n], even x ]

guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter f xs = do
  x <- xs
  guard $ f x
  return x

guardFilterComp :: (a -> Bool) -> [a] -> [a]
guardFilterComp f xs = [ x | x <- xs, f x ]

evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard $ even nSquared
  return nSquared

evenSquaresComp :: [Int]
evenSquaresComp =
  [ nSquared | n <- [0 .. 9], let nSquared = n ^ 2, even nSquared ]

reservoirDogs :: [String]
reservoirDogs =
  [ mrName
  | name <- ["brown", "blue", "pink", "orange"]
  , let mrName = "Mr. " ++ (\(x : xs) -> toUpper x : xs) name
  ]
