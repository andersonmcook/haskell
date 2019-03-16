import Control.Monad
import System.Environment

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead = if length args > 0 then read $ head args else 0
--   numbers <- replicateM linesToRead getLine
--   let ints = map read numbers :: [Int]
--   print $ sum ints

  {-
  mapM -- takes an IO action and a regular list, performing the action on each item in the list
          and returning a list in the IO context

  mapM_ -- same as mapM, but it throws away the values

  replicateM -- takes an IO action, an Int n, and then repeats the IO action n times, returning
                the results in an IO list

  replicateM_ -- same as replicateM, but it throws away the results
  -}

reverser :: IO ()
reverser = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed

sampleData :: [Char]
sampleData = ['6', '2', '\n', '2', '1', '\n']

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print $ sum numbers

mainSumSquares :: IO ()
mainSumSquares = do
  userInput <- getContents
  let numbers = map (^ 2) (toInts userInput)
  print $ sum numbers

-- Q22.1
calc :: [String] -> Int
calc (val1 : "*" : val2 : _) = read val1 * read val2
calc (val1 : "+" : val2 : _) = read val1 + read val2
calc _                       = 0

simpleCalc :: IO ()
simpleCalc = do
  userInput <- getContents
  print . calc $ lines userInput

-- Q22.2 go back and do this
