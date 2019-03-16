{-
importing modules...

can have namespace clashes, which is where some of these come in handy

import Data.List -- all functions
import Data.List (nub, sort) -- just these two
import Data.List hiding (nub) -- all functions except this one
import qualified Data.Map -- have to use Data.Map.whatever to use that function
import qualified Data.Map as M -- have to use M.whatever to use that function

-}
