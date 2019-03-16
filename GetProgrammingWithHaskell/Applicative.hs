import qualified Data.Map as Map

type LatLng = (Double, Double)

locationDB :: Map.Map String LatLng
locationDB = Map.fromList
  [ ("Arkham"   , (42.6054, -70.7829))
  , ("Innsmouth", (42.8250, -70.8150))
  , ("Carcosa"  , (29.9714, -907694))
  , ("New York" , (40.7776, -73.9891))
  ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLngToRads :: LatLng -> LatLng
latLngToRads (lat, lng) = (toRadians lat, toRadians lng)

haversine :: LatLng -> LatLng -> Double
haversine coords1 coords2 = earthRadius * c
 where
  (rlat1, rlng1) = latLngToRads coords1
  (rlat2, rlng2) = latLngToRads coords2
  dlat           = rlat2 - rlat1
  dlng           = rlng2 - rlng1
  a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlng / 2)) ^ 2
  c              = 2 * atan2 (sqrt a) (sqrt (1 - a))
  earthRadius    = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing         = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

{-
<*> is pronounced "app"
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
f means any type that's an instance of Applicative, for example: Maybe
Maybe (Int -> Double) -> Maybe Int -> Maybe Double

takes a function in a context
-}

-- partial application
maybeInc :: Num a => Maybe (a -> a)
maybeInc = (+) <$> Just 1
-- maybeInc <*> Just 5 -- Just 6
-- maybeInc <*> Noting -- Nothing

justCatsAndDogs :: Maybe String
justCatsAndDogs = (++) <$> Just "cats" <*> Just " and dogs" -- Just "cats and dogs"

val1 :: Maybe Int
val1 = Just 10

val2 :: Maybe Int
val2 = Just 5

added :: Maybe Int
added = (+) <$> val1 <*> val2 -- could replace (+) with div or mod or any binary function

-- 28.2.2
startingCity :: Maybe LatLng
startingCity = Map.lookup "Carcosa" locationDB

destCity :: Maybe LatLng
destCity = Map.lookup "Innsmouth" locationDB

distance :: Maybe Double
distance = haversine <$> startingCity <*> destCity
{-
[haversine <$> startingCity]
this is partial application which gives us a function of type Maybe (LatLng -> Double)

<*> operator takes a function in a context, in this case Maybe (LatLng -> Double)
and an argument in the same context Maybe LatLng and applies the function to that argument,
returning a type still in the context, a Maybe Double
-}

-- 28.6

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

-- 28.2.3

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 $ min v2 v3

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

minMain :: IO ()
minMain = do
  putStrLn "Enter three numbers"
  minInt <- minOfInts
  putStrLn $ mconcat [show minInt, " is the smallest"]

-- 28.3
{-
Create a User in different contexts (Maybe, IO) without redefining what a User is
-}

data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

user :: Maybe User
user = User <$> serverUsername <*> serverGamerId <*> serverScore

readIntFromLine :: IO Int
readIntFromLine = read <$> getLine

userMain :: IO ()
userMain = do
  putStrLn "Enter a username, gamerId, and score"
  user <- User <$> getLine <*> readIntFromLine <*> readIntFromLine
  print user

-- Q28.2
haversineIO :: IO LatLng -> IO LatLng -> IO Double
haversineIO ll1 ll2 = haversine <$> ll1 <*> ll2
