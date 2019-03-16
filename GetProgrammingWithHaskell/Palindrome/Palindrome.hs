module Palindrome (isPalindrome) where

import Data.Char (toLower, isSpace, isPunctuation)

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preProcess :: String -> String
preProcess = toLowerCase . stripPunctuation . stripWhiteSpace

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
  where cleanText = preProcess text
