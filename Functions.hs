{-
can define separate function bodies for different patterns
(pattern matching works like you're used to)
when making patterns, provide a catch-all to prevent crashes
_ as a parameter

indentation is important, it continues functions

"as patterns"
to pattern match and simultaneously keep a reference to a full list, for example
someFuncOnLists theList@(x:xs) = theList -- theList is the whole list, x is the head, xs is the tail

Guards
kind of like cond, provide a predicate function and if it's True, do the corresponding code block, else try the next
have a catch-all at the end

follows a function's name and its parameters

<= is less than or equal to, not anything fancy
with guards, don't put = after the parameters

bmiTell :: (RealFloat a) => a -> String
bmiTell weight height
    | bmi <= skinny = "skinny"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise   = "huge"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
    {- or -}
          (skinny, normal, fat) = (18.5, 25.0, 30.0) -- pattern-matching on a tuple/triple


You can define infix functions with backticks

a `someInfixFunction` b = a * b

$ for function application
$ has the lowest precedence of operators so it is effectively done last
$ is right associative
function application with a space is left associative
-- first double everything in the range, then keep results greater than 10, then sum
sum (filter (> 10) (map (*2) [2..10]))
becomes
sum $ filter (> 10) $ map (*2) [2..10]
-}
