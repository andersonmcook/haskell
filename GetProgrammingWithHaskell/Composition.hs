import Data.Semigroup

-- <> combining instances of the same time
-- can implement Semigroup for Integer by defining <> as +
-- type class laws require certain behavior like associativity

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Clear deriving (Eq, Show)

instance Semigroup Color where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) Clear b = b
  (<>) a Clear = a
  (<>) a b | a == b = a
           | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
           | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend = (<>)

{-
class Monoid a where
  mempty :: a            -- empty Monoid
  mappend :: a -> a -> a -- add an element
  mconcat :: [a] -> a    -- concat multiple Monoids

x = [1, 2, 3]
x ++ [] == x
x <> [] == x
x mappend mempty == x


Monoid Laws
1. mappend mempty x is x
2. mappend x mempty is x
3. associativity, mappend x (mappend y z) is mappend (mappend x y) z
4. mconcat = foldr mappend mempty , ie foldr (\x y -> mappend x y) mempty
-- foldr because of infinite lists
-}

-- type Events = [String]
data Events = Events [String]
-- type Probs = [Double]
data Probs = Probs [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
 where
  totalProbs      = sum probs
  normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
 where
  nToAdd     = length l2
  repeatedL1 = map (take nToAdd . repeat) l1
  newL1      = mconcat repeatedL1
  cycledL2   = cycle l2

combineEvents :: Events -> Events -> Events
-- combineEvents e1 e2 = cartCombine combiner e1 e2
--   where combiner = (\x y -> mconcat [x, "-", y])
combineEvents (Events e1) (Events e2) =
  Events (cartCombine (\x y -> mconcat [x, "-", y]) e1 e2)

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mappend = (<>)
  mempty = Events []

combineProbs :: Probs -> Probs -> Probs
-- combineProbs p1 p2 = cartCombine (*) p1 p2
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mappend = (<>)
  mempty Probs []

instance Semigroup PTable where
  (<>) pTable1 (PTable [] []) = pTable1
  (<>) (PTable [] []) pTable2 = pTable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable (combineEvents e1 e2) (combineProbs p1 p2)

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]
