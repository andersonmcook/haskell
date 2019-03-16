{-
Arguments from most to least general
-}
getRequestURL :: String -> String -> String -> String -> String
getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder :: String -> (String -> String -> String -> String)
genHostRequestBuilder host =
  \apiKey resource id -> getRequestURL host apiKey resource id

exampleURLBuilder :: (String -> String -> String -> String)
exampleURLBuilder = genHostRequestBuilder "http://example.com"

genHostRequestBuilderNoLambda :: String -> String -> String -> String -> String
genHostRequestBuilderNoLambda host = getRequestURL host

exampleURLBuilderNoLambda :: String -> String -> String -> String
exampleURLBuilderNoLambda = genHostRequestBuilderNoLambda "http://example.com"

exampleURL :: String -> String
exampleURL = getRequestURL "http://example.com" "1337hAsk3ll" "books"

-- also known as Prelude.flip
flipBinaryArgs :: (a -> b -> c) -> (b -> a -> c)
flipBinaryArgs f = \x y -> f y x

binaryPartialApplication :: (a -> b -> c) -> a -> b -> c
binaryPartialApplication f a = f a
