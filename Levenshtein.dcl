definition module Levenshtein

class levenshtein a :: !a !a -> Int

instance levenshtein String, Char, [Char]

