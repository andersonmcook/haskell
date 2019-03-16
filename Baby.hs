doubleMe x = x + x

-- doubleUs x y = x * 2 + y * 2
doubleUs x y = doubleMe x + doubleMe y

-- functions can be defined in any order

-- /= not equal

doubleIfLessThan100 n = if n > 100 then n else doubleMe n

-- use `` around a function to make it infix: 1 `elem` [1,2] == True, elem 1 [1,2] == True

-- lists are homogenous
-- strings are character lists, ['h', 'i'] == "hi"

-- ++ concat, strings or lists, [] ++ []
-- : prepend, strings or lists, 5:[] == [5], [1]:[] == [[1]]
-- !! to get element out of list at index _ ['a', 'b', 'c'] !! 0 == 'a'
-- lists can be compared with ==, >, <, >=, <=
-- head, tail, init, last, length, reverse, (take count list), (drop ct list) ...functions to work on lists
-- null [] to check if a list is empty
-- more list functions: maximum, minimum, sum, product, `elem`

-- can make ranges [1..20], ['a'..'z'], ['A'..'z'], can make infinite ranges [1..], ['a'..]
-- ranges can specify step [0, 3..20] first number, next number..end [0,3,6,9,12,15,18]
-- backwards list [20, 19..1]
-- cycle takes a list and cycles through it infinitely, have to use take to get it to stop.. take 10 (cycle [1,2,3])
-- repeat takes an element and repeats it infinitely
-- replicate takes an element and repeats it on count, replicate 3 10 == [10,10,10]

{-
list comprehensions:
need to use same variable both places
[x * 2 | x <- [1..10]] == [2,4,6,8,10,12,14,16,18,20]
can have a filter predicate (or multiple comma separated predicates that an item must pass for all)
[x * 2 | x <- [1..10], x * 2 >= 12] == [12,14,16,18,20]
can use two lists
[x * y | x <- [1, 2, 3], y <- [4, 5]] == [4,5,8,10,12,15]

fizzbuzz
[ if x `mod` 15 == 0 then "fizzbuzz" else if x `mod` 5 == 0 then "buzz" else if x `mod` 3 == 0 then "fizz" else show x | x <- [1..100]]

composition
isOdd n = not (even n) -- no composition
isOdd n = (not . even) n -- composition
isOdd n = not $ even n -- forward application (removes parentheses)

rewrite length
length' xs = sum [1 | _ <- xs]
length' [1, 2, 3] == 3
length' "hello" == 5
-}
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs
{- elixir
defmodule A do
  def sum([]), do: 0
  def sum([x|xs]), do: x + sum(xs)
end
-}
{- es6
const sum = ([x, ...xs], count = 0) => typeof x !== 'undefined' ? sum(xs, count + x) : count
const sum = ([x, ...xs]) => typeof x !== 'undefined' ? x + sum(xs) :
-}

{-
foldl, like reduce, from the left
func initial list
foldl (+) 0 [1, 2, 3, 4, 5] == 15
there is also foldr, which works from the right

tuples (1, "2", "three"), are heterogenous
fst ("a", "b") == "a" (only works on pairs)
snd ("a", "b") == "b" (only works on pairs)

zip turns 2 lists into a list of pairs (2 element tuples)
zipping lists of different lengths matches the length of the smaller list

can't compare between tuples of different shapes (1, "1") /= ("1", 1)
each size of tuple is a different type (pair, triple, 4-tuple)

TYPES
Int: bounded integer
Integer: unbounded integer
Float: single precision
Double: float with double precision
Bool: boolean
Char: character

type variable a, [a], (a, b)

TYPECLASSES
Functions are not part of Eq, Ord, Show

(==) :: (Eq a) => a -> a -> Bool
(Eq a) is the class constraint
Eq typeclass provides an interface for testing equality
all standard types in Haskell except for IO are part of the Eq typeclass
==, /=

Ord is for types that have an ordering
>, >=, <, <=

Ordering is a type that can be GT, LT, or EQ
all Ord are part of Eq

Show - all members can be represented as strings
show is a function to transform input into a string

Read - opposite of Show typeclass
read - opposite of show function, takes a string, returns a type

need to "do" something with read "5" + 5 so Haskell knows what type it is, else
we can use explicit type annotations :: with read
read "5" :: Int
5
read "[1,2,3,4]" :: [Int]
[1,2,3,4]

Enum - sequentially ordered types, can be enumerated
can use its types in list ranges
have defined successors and predecessors
functions: succ, pred to get successor and predecessor for an Enum
types in Enum: (), Bool, Char, Ordering, Int, Integer, Float, Double

Bounded - members have an upper and lower bound

Num - act like numbers, Int, Integer, Float, Double
must be a part of Show and Eq

Integral - Int, Integer

Floating - Float, Double
-}
