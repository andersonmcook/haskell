-- counts lines
-- main = interact wordCount where wordCount input = show (length (lines input)) ++ "\n"
main = interact wordCount
  where wordCount input = (++ "\n") . show . length . lines $ input
