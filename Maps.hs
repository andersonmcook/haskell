{-
work with maps with Data.Map
represented as list of tuples

map = fromList [("a", 1) ,("b", 2) ,("c", 3)]

duplicate keys are overwritten

Data.Map exports functions that class with Prelude and Data.List
so I should use a qualified import
import qualified Data.Map as Map

keys need to be Ord (orderable)

can't just define a map like a list of tuples
need to use fromList


-}
