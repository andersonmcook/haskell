import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  size1 <- getLine
  putStrLn "what is the cost of pizza 1?"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2?"
  size2 <- getLine
  putStrLn "what is the cost of pizza 2?"
  cost2 <- getLine
  let pizza1      = (read size1, read cost1)
  let pizza2      = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn $ describePizza betterPizza

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2 then p1 else p2
 where
  costP1 = costPerInch p1
  costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = mconcat
  [ "The "
  , show size
  , " pizza "
  , "is cheaper at "
  , show $ costPerInch (size, cost)
  , " per square inch"
  ]

-- 21.3.1
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1      = (size1, cost1)
  let pizza2      = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza
-- return takes a value of a type and puts it back in the context of the do-notation (String is returned as a Maybe String)

-- Q21.1
{-
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn Statement
-}
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

helloMain :: Maybe String
helloMain = do
  name <- Map.lookup 1 (Map.fromList [(1, "Anderson")])
  return $ helloPerson name

-- Q21.2
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibMain :: IO ()
fibMain = do
  putStrLn "How many numbers in the Fibonacci sequence do you want?"
  n <- getLine
  putStrLn . show . fib $ read n
