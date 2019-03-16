import Data.Char

remove :: (a -> Bool) -> [a] -> [a]
remove _ []       = []
remove f (x : xs) = if f x then remove f xs else x : remove f xs

product' :: [Int] -> Int
product' = foldl (*) 1

elem' :: Eq a => a -> [a] -> Bool
-- elem' a xs = length (filter (\x -> x == a) xs) /= 0
elem' a = foldr (\curr acc -> acc || curr == a) False

isPalindrome :: String -> Bool
isPalindrome str = reverse s == s where s = map toLower (filter isAlpha str)
