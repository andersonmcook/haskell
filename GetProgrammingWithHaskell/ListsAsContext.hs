{-
  The Applicative type class allows you to use functions that are inside a context, such as Maybe or IO.
  Functor is a superclass of Applicative (all Applicatives are Functors)

  two required methods for the Applicative type class
  (<*>) :: f (a -> b) -> f a -> f b
    takes a function in a Functor and a value in the same Functor and applies the function to the value
  pure :: a -> f a
    takes a normal type and puts it in the context of a functor
 -}

maybeConcat :: Maybe String -> Maybe String -> Maybe String
maybeConcat s1 s2 = (++) <$> s1 <*> s2

-- add 6 to Just 5 using fmap
just11A :: Maybe Int
just11A = (6 +) <$> Just 5

-- add 6 to Just 5 using 'ap'
just11B :: Maybe Int
just11B = pure (6 +) <*> Just 5

-- type signature alone gives it enough information on what context to put it in
helloWorld :: IO String
helloWorld = pure "Hello World"

{-
Parameterized types that represent a container are types that represent a data structure.

When a type is a context, extra information is implied about the type, beyond its structure.

The two tuple (,) and Data.Map are instances of Functor, but not Applicative.

Lists as context represent nondeterministic computing.
-}

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

-- deterministic -- also doesn't work?
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- Primes

-- simple but inefficient
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
 where
  twoThroughN    = [2 .. n]
  composite      = pure (*) <*> twoThroughN <*> twoThroughN
  isNotComposite = not . (`elem` composite)

-- Generating Test Data

data User =  User {
  name :: String,
  gamerId :: Int,
  score :: Int
} deriving Show

testNames :: [String]
testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  ]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

-- Q29.1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f x = pure f <*> x

-- Q29.2
example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6


-- Q29.3
