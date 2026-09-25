module V09 where

import Test.QuickCheck

prop_binomi :: Integer -> Integer -> Bool
prop_binomi a b = (a + b) ^ 2 == a ^ 2 + 2 * a * b + b ^ 2
