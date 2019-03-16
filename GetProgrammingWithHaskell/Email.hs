main :: IO ()
main = do
  print "Who is this email for?"
  recipient <- getLine
  print "What is the title?"
  title <- getLine
  print "Who is the author?"
  author <- getLine
  print $ createEmail recipient title author

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart title = "Thanks for buying " ++ title ++ ".\n"

fromPart :: String -> String
fromPart = (++) "Thanks,\n"

createEmail :: String -> String -> String -> String
createEmail recipient title author =
  toPart recipient ++ bodyPart title ++ fromPart author

