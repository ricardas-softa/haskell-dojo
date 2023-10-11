module Sandbox where

import Data.Char (toUpper)

_MY_CONSTANT_ = 'A'

identity x = x

upper = toUpper . identity