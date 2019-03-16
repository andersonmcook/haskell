{-
data Maybe a = Nothing | Just a
Maybe is a _type_ constructor
Nothing & Just are _data_ constructors

Maybe Int -> Maybe container holding an Int (or Nothing)
Maybe String -> Maybe container holding a String (or Nothing)

Functors apply a function to a wrapped value
Applicatives apply a wrapped function to a wrapped value
Monads apply a function that returns a wrapped value to a wrapped value

-}
