-- quicksort :: -- comment
--     (Collection t, Stack t, Semigroup t, Ord a) => 
--     t a -> t a
-- quicksort [] = []
-- quicksort (x : xs) = {
--     ltx = (y -> y <= x) `filter` xs
--     gtx = (y -> y >= x) `filter` xs 
--     (quicksort ltx) `mappend` [x] `mappend` (quicksort gtx)
-- }

someComputation = 5.0 + 5 + 10.52 + 10.0000 + 91