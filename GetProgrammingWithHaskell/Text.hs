{-
Text is preferred over String for performance reasons (faster, more memory efficient)
String is a List
Text is an Array

Text doesn't use lazy evaluation
if you need lazy evaluation use Data.Text.Lazy

pack :: String -> Text
unpack :: Text -> String
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.IO as Text.Lazy.IO

firstWord :: String
firstWord = "pessimism"

secondWord :: Text.Text
secondWord = Text.pack firstWord

thirdWord :: String
thirdWord = Text.unpack secondWord

fourthWord :: Text.Text
fourthWord = Text.pack thirdWord


-- use language extension when compiling with GHC
-- > ghc text.hs -XOverloadedStrings
-- or in code
-- {-# LANGUAGE <ExtensionName> #-}

-- notice the type is not String
aWord :: Text.Text
aWord = "Cheese"


{-
other language extensions

ViewPatterns - allows for more sophisticated pattern matching

TemplateHaskell - provides tools for Haskell metaprogramming

DuplicateRecordFields - solves the problem of using the same field name for different types using record syntax

NoImplicitPrelude - allows you to not use the default Prelude
-}

combinedTextMonoid :: Text.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: Text.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- pick back up at 23.3
dharma :: Text.Text
dharma = "धर्म"

bgText :: Text.Text
bgText
  = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात् ।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: Text.Text -> Text.Text -> Text.Text
highlight query fullText = Text.intercalate highlighted pieces
 where
  pieces      = Text.splitOn query fullText
  highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = do
  Text.IO.putStrLn $ highlight dharma bgText

-- Q23.1
{-
use Text instead of String

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  putStrln $ helloPerson name
-}
helloPerson :: Text.Text -> Text.Text
helloPerson name = mconcat ["Hello ", name, "!"]

textMain :: IO ()
textMain = do
  Text.IO.putStrLn "Hello! What's your name?"
  name <- Text.IO.getLine
  Text.IO.putStrLn $ helloPerson name

-- Q23.2
{-
toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)
-}

toInts :: Text.Lazy.Text -> [Int]
toInts = map (read . Text.Lazy.unpack) . Text.Lazy.lines

lazyMain :: IO ()
lazyMain = do
  userInput <- Text.Lazy.IO.getContents
  let numbers = toInts userInput
  Text.Lazy.IO.putStrLn ((Text.Lazy.pack . show . sum) numbers)
