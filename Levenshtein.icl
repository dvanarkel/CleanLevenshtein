implementation module Levenshtein

import StdEnv

instance levenshtein Char where levenshtein c1 c2 = if (c1==c2) 0 1

instance levenshtein [Char]
where
    // Algorithm from https://rosettacode.org/wiki/Levenshtein_distance#Haskell
    levenshtein s1 s2 = last (foldl transform [0..length s1] s2)
    where
        transform ns=:[n:ns`] c = scan (calc c) (n+1) (zip3 s1 ns ns`)
        calc c z (c`, x, y) = minList [y+1, z+1, toInt (c`<>c) + x]

instance levenshtein String
where levenshtein s1 s2 = levenshtein [c \\ c <-: s1] [c \\ c <-: s2]

instance toInt Bool where toInt True = 1; toInt False = 0

zip3 :: [a] [b] [c] -> [(a,b,c)]
zip3 as bs cs = [(a,b,c) \\ a <- as & b <- bs & c <- cs]

Start = levenshtein "kitten" "sitting"

