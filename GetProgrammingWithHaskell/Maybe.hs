import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

data Organ = Heart | Brain | Kidney | Spleen deriving (Eq, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map (\id -> Map.lookup id catalog) ids

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length $ filter (\o -> o == Just organ) available

-- same as Data.Maybe.isJust
isSomething :: Maybe Organ -> Bool
isSomething (Just _) = True
isSomething Nothing  = False

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = List.intercalate ", " organList

numOrZero :: Maybe Int -> Int
-- numOrZero Nothing = 0
-- numOrZero (Just n) = n
numOrZero = fromMaybe 0

-- 19.4
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer organ = case organ of
  Brain -> Vat Brain
  Heart -> Cooler Heart
  organ -> Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation c@(Vat    a) = (Lab, c)
placeInLocation c@(Cooler a) = (Lab, c)
placeInLocation c@(Bag    a) = (Kitchen, c)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (l, c) = show c ++ " in the " ++ show l

processAndReport :: (Maybe Organ) -> String
processAndReport Nothing      = "error, id not found"
processAndReport (Just organ) = report $ process organ

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport (Map.lookup id catalog)


-- 19.1 challenge
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter isNothing

-- 19.2 challenge
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just a) = Just (f a)
maybeMap _ Nothing  = Nothing
