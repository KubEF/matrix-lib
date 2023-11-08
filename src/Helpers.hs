module Helpers where
import GenTH

takeFst :: (a, b, c) -> a
takeFst (a, _, _) = a

takeSnd :: (a, b, c) -> b
takeSnd (_, b, _) = b

takeThrd :: (a, b, c) -> c
takeThrd (_, _, c) = c


roundUp :: Double -> Int
roundUp x = if x > toEnum (round x) then toEnum $ round x + 1 else toEnum $ round x

-- перестановки с повторениями: для наборов [a1, a2] [b1, b2, b3] вернёт [[a1,a2,b1,b2],[a1,b1,a2,b2],[a1,b1,b2,a2],[b1,a1,a2,b2],[b1,a1,b2,a2],[b1,b2,a1,a2]]
anagrams :: [a] -> [a] -> [[a]]
anagrams l1 l2 = case (l1, l2) of
    ([], []) -> [[]]
    (list1, []) -> map (head list1 :) (anagrams (tail list1) [])
    ([], list2) -> map (head list2 :) (anagrams [] (tail list2))
    (list1, list2) -> map (head list1 :) (anagrams (tail list1) list2) ++ map (head list2 :) (anagrams list1 (tail list2))