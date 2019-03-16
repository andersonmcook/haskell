import qualified Data.Map as Map

-- () represents context/container
-- input type, function, output type

-- context: IO, Maybe
-- container: List, Map

-- Functor solves (a) a -> b (b)
-- where the function input and output are not in a context/container

-- Applicative solves (a) (a) -> b (b)
-- where the function output is not in a context/container

-- Applicative also solves (a) (a -> b) (b)
-- where the entire function is in a context/container

-- Monad solves (a) a -> (b) (b)
-- where the argument to a function isn't in a context/container

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- Functor type class requires definition of fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
--  function from a to b -> Functor a -> Functor b
-- <$> is a synonym for fmap, except it's a binary operator

-- basically don't have to think about pattern matching on (Just n) or Nothing

-- if you have a function of (a -> b -> c) then it won't work with Functor/fmap

-- List, Map, Maybe, and IO are all members of Functor

-- instance Functor Maybe where
--   fmap f (Just n) = Just (f n)
--   fmap _ Nothing  = Nothing

successStr :: Maybe String
successStr = fmap show successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

-- 27.3
data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { name        = "left arm"
  , description = "left arm for face punching!"
  , cost        = 1000.00
  , count       = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name        = "right arm"
  , description = "right arm for kind hand gestures!"
  , cost        = 1025.00
  , count       = 5
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name        = "robot head"
  , description = "this head looks mad"
  , cost        = 5092.25
  , count       = 2
  }

type Html = String

{-
all return the same result

(g . f) x
g . f $ x
g (f x)
g $ f x
-}

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
  [ "<h2>"
  , name part
  , "</h2>"
  , "<p><h3>desc</h3>"
  , description part
  , "</p><p><h3>cost</h3>"
  , show . cost $ part
  , "</p><p><h3>count</h3>"
  , show . count $ part
  , "</p>"
  ]

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList $ zip [1, 2, 3] [leftArm, rightArm, robotHead]

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
-- allParts = map snd $ Map.toList partsDb
allParts = snd <$> Map.toList partsDB

-- fmap for a List is the regular map function
-- Functor type class = things that can be mapped over

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDb :: Map.Map Int Html
htmlPartsDb = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box a) = Box (f a)

morePresents :: Box a -> Int -> Box [a]
-- morePresents (Box a) n = Box (replicate n a)
morePresents box n = replicate n <$> box

-- Q27.2
boxInBox :: Box (Box Int)
boxInBox = Box <$> Box 1

unwrap :: Box (Box a) -> Box a
unwrap (Box a) = a

-- Q28.3
printCost :: Maybe Double -> IO ()
printCost (Just cost) = print cost
printCost Nothing     = putStrLn "item not found"

main :: IO ()
main = do
  putStrLn "enter a part number"
  partNo <- getLine
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
