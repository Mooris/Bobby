module Utils where

import           Numeric

if' :: a -> a -> Bool -> a
if' tru fls cnd = if cnd then tru else fls

prettyHex :: (Show a, Integral a, Num a) => a -> String
prettyHex = ("0x" ++) . flip showHex ""

    