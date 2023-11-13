module Helpers where

isPowOf2 :: Int -> Bool
isPowOf2 k = logBase 2 size == truncated
    where
        size = fromIntegral k :: Double
        truncated = toEnum (truncate $ logBase 2 size) :: Double

readSpecDouble :: String -> Double
readSpecDouble a
    | head a == '.' = read ('0' : a)
    | head a == '-' && (a !! 1) == '.' = read ("-0" ++ tail a)
    | head a == 'e' = read ('1' : a)
    | head a == '-' && (a !! 1) == 'e' = read ("-1" ++ tail a)
    | otherwise = read a

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f ~(x, y, z, w) = f x y z w

takeFst :: (a, b, c) -> a
takeFst (a, _, _) = a

takeSnd :: (a, b, c) -> b
takeSnd (_, b, _) = b

takeThrd :: (a, b, c) -> c
takeThrd (_, _, c) = c

roundUp :: Double -> Int
roundUp x = if x > toEnum (round x) then toEnum $ round x + 1 else toEnum $ round x

maybeAdd :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeAdd x y = case (x, y) of
    (Just u, Just v) -> Just $ u + v
    (Nothing, Nothing) -> Just 0
    (Just u, Nothing) -> Just u
    (Nothing, Just v) -> Just v

maybeSubtract :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeSubtract x y = case (x, y) of
    (Just u, Just v) -> Just $ u - v
    (Nothing, Nothing) -> Just 0
    (Just u, Nothing) -> Just u
    (Nothing, Just v) -> Just (-v)

-- перестановки с повторениями: для наборов [a1, a2] [b1, b2, b3] вернёт [[a1,a2,b1,b2],[a1,b1,a2,b2],[a1,b1,b2,a2],[b1,a1,a2,b2],[b1,a1,b2,a2],[b1,b2,a1,a2]]
anagrams :: [a] -> [a] -> [[a]]
anagrams l1 l2 = case (l1, l2) of
    ([], []) -> [[]]
    (list1, []) -> map (head list1 :) (anagrams (tail list1) [])
    ([], list2) -> map (head list2 :) (anagrams [] (tail list2))
    (list1, list2) -> map (head list1 :) (anagrams (tail list1) list2) ++ map (head list2 :) (anagrams list1 (tail list2))