{-
define a type with `data` keyword
data Bool = True | False

value constructors are functions that return a value of the data type
we can map them and partially apply them
Circle and Rectangle are value constructors below

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
... have to add `deriving (Show)` to be able to print to console

Circle :: Float -> Float -> Float -> Shape

can pattern match on constructors but not define a function as taking their type, because they don't have a type
ie, can't do... someFunc :: Circle -> Float
but can do...   someFunc Circle = ...

-- define a Point and use that to cut down on code
data Point = Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point

can export data types and constructors in modules

-- use (..) to export all constructors
-- omit to just export the data type
module Shapes
( Point(..)
, Shape(Triangle, Square)
, ThirdThing
)

Record Syntax

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

this also automatically creates functions to access the properties (firstName, lastName, age, etc.)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}

don't have to define them in order to create a new one

Maybe is a type constructor

an empty list can act like a list of anything
that's why you can do [1, 2, 3] ++ []

strong convention not to add typeclass constraints in data declarations
-}
