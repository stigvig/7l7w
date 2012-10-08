allEven1 :: [Integer] -> [Integer]
allEven1 l = [i | i <- l, even i]

allEven2 :: [Integer] -> [Integer]
allEven2 = filter even

-- allEven3 :: [Integer] -> [Integer]
-- allEven3 = []
-- allEven3 (h:t)
--   | even h    = h:allEven3 t
--   | otherwise = allEven3 t

mreverse l = foldl (\acc x -> x : acc) [] l

allCombinations1 :: (Ord t) => [t] -> [(t, t)]
allCombinations1 l = [(a, b) | a <- l, b <- l, a < b || a == b]

allCombinations2 [] = []
allCombinations2 (h:t) =
  comb h t ++ allCombinations2 t
  where
    comb a [] = [(a, a)]
    comb a (h:t) = (a, h) : comb a t

data Color = Black | White | Blue | Yellow | Red
           deriving (Show, Ord, Eq)
colors = [Black, White, Blue, Yellow, Red]

colorComb1 = allCombinations1 colors
colorComb2 = allCombinations2 colors

multTable = [(l, r, l * r) | l <- [0..12], r <- [0..12]]