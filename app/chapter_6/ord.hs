module Ordnung where

-- λ> max (length [1,2,3])
--     (length [8,9,10,11,12])
-- Some flags have not been recognized: prompt2,
-- Prelude| Prelude| Prelude| λ>
-- λ>
-- λ> max (length [1,2,3])
--     (length [8,9,10,11,12])
-- Some flags have not been recognized: prompt2,
-- Prelude| Prelude| Prelude| 5
-- Some flags have not been recognized: prompt2, λ|
-- λ> max (length [1,2,3]) (length [8,9,10,11,12])
-- 5
-- λ> compare (3*4) (3*5)
-- LT
-- λ> compare "Julie" True

-- <interactive>:23:17-20: error:
--     • Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
--     • In the second argument of ‘compare’, namely ‘True’
--       In the expression: compare "Julie" True
--       In an equation for ‘it’: it = compare "Julie" True
-- λ> (5 + 3) (3 + 6)

-- <interactive>:24:1-15: error:
--     • Non type-variable argument in the constraint: Num (t1 -> t2)
--       (Use FlexibleContexts to permit this)
--     • When checking the inferred type
--         it :: forall t1 t2. (Num t1, Num (t1 -> t2)) => t2
-- λ> (5 + 3) > (3 + 6)
-- False
