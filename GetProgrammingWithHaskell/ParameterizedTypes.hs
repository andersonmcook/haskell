import qualified Data.Map as Map

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box a) = a

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box a) = Box (f a)

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple a b c) = Triple (f a) (f b) (f c)

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple a _ _) = a

second :: Triple a -> a
second (Triple _ a _) = a

third :: Triple a -> a
third (Triple _ _ a) = a

toList :: Triple a -> [a]
toList (Triple a b c) = [a, b, c]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple a b c) = Triple (f a) (f b) (f c)

data List a = Empty | Cons a (List a) deriving Show

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty       = Empty
ourMap f (Cons x xs) = Cons (f x) (ourMap f xs)

data Organ = Heart | Brain | Kidney | Spleen deriving (Enum, Eq, Ord, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map (\o -> length $ filter (== o) values) allOrgans

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)
