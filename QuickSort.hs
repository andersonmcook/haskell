qs [] = []
qs (x : xs) =
  let smaller = qs [ a | a <- xs, a <= x ]
      bigger  = qs [ a | a <- xs, a > x ]
  in  smaller ++ x : bigger



