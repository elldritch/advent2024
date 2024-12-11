module Advent.Math (digits) where

import Relude

digits :: (Integral b, Integral a) => a -> b
digits n = floor @Double (logBase 10 $ fromIntegral n) + 1
