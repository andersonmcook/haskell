-- stack runghc LearningHaskell/Program.hs
main :: IO ()
main = do
  content <- readFile "LearningHaskell/numbers.txt"
  putStrLn content
