-- sum types are types that are made by combining other types with an "or"
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName -- "Anderson Cook"
          | NameWithMiddle FirstName MiddleName LastName -- "Anderson McCall Cook"
          | TwoInitialsWithLast Char Char LastName -- "H.P. Lovecraft"
          | FirstNameWithTwoInitials FirstName Char Char -- "Andrew W.K."

data Author = Author Name

data Artist = Person Name
            | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Book = Book {
  author :: Creator,
  isbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double
}

data VinylRecord = VinylRecord {
  artist :: Creator,
  recordTitle :: String,
  recordYear :: Int,
  recordPrice :: Double
}

{-
"bookPrice" and "recordPrice" are not just named "price" because that would define 2 functions named "price"
(because record syntax defines helper functions for us)
which, apparently, is a failing of Haskell that has a later workaround
-}

data CollectibleToy = CollectibleToy {
  name :: String,
  toyDescription :: String,
  toyPrice :: Double
}

data Pamphlet = Pamphlet {
  title :: String,
  pamphletDescription :: String,
  contact :: String
}

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem     book  ) = bookPrice book
price (RecordItem   record) = recordPrice record
price (ToyItem      toy   ) = toyPrice toy
price (PamphletItem _     ) = 0.0

type Radius = Double
type Height = Double
type Width = Double

data Shape = Circle Radius
           | Square Height
           | Rectangle Height Width deriving Show

perimeter :: Shape -> Double
perimeter (Circle r     ) = 2 * pi * r
perimeter (Square h     ) = 4 * h
perimeter (Rectangle h w) = 2 * h + 2 * h

area :: Shape -> Double
area (Circle r     ) = pi * r ^ 2
area (Square h     ) = h ^ 2
area (Rectangle h w) = h * w
