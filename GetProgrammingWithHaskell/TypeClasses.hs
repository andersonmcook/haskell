class Describable a where
  describe :: a -> String

-- defining a type class with hand-rolled instances
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- instance Show SixSidedDie where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

-- instance Eq SixSidedDie where
--   (==) S1 S1 = True
--   (==) S2 S2 = True
--   (==) S3 S3 = True
--   (==) S4 S4 = True
--   (==) S5 S5 = True
--   (==) S6 S6 = True
--   (==) _ _ = False

-- instance Ord SixSidedDie where
--   compare S6 S6 = EQ
--   compare S6 _ = GT
--   compare _ S6 = LT
--   compare S5 S5 = EQ
--   compare S5 _ = GT
--   compare _ S5 = LT
--   -- ...

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Show)

instance Enum SixSidedDie where
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 3
  fromEnum S5 = 4
  fromEnum S6 = 5

  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No such value"

-- different way to define a type, can have only one type constructor and one type
newtype Name = Name (String, String) deriving (Eq, Show)

data Number = One | Two | Three deriving (Enum)

instance Eq Number where
  (==) n1 n2 = (fromEnum n1) == (fromEnum n2)

instance Ord Number where
  compare n1 n2 = compare (fromEnum n1) (fromEnum n2)

data FourSidedDie = S1 | S2 | S3 | S4 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where roll :: Int -> a

instance Die FourSidedDie where roll n = toEnum (n `mod` 4)
