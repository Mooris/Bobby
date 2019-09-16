import           Data.Array.IO

toto :: IOArray (Int, Int) Int
toto = newArray ((0, 0), (160, 144)) 0

