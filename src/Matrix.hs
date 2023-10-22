{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Matrix where

{- TODO:
[X] multiply
[X] взятие подматрицы
[X] map2
[X] map (ну, fmap)
[X] filter
-}
newtype Matrix a = Matrix [[a]] deriving (Foldable, Traversable, Eq)

instance (Show a) => Show (Matrix a) where
    show :: Matrix a -> String
    show (Matrix x) = concatMap (("\n" ++) . show) x ++ "\n"

maybeFunc2 :: (t1 -> t2 -> b) -> Maybe t1 -> Maybe t2 -> Maybe b
maybeFunc2 f x y = x >>= (\a -> y >>= (Just . f a))

maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter _ Nothing = Nothing
maybeFilter predicate (Just a)
    | predicate a = Just a
    | otherwise = Nothing

-- map2 :: (a1 -> a2 -> b) -> Matrix a1 -> Matrix a2 -> Matrix b
-- map2 func x y = fmap func x <*> y

filterMMaybe :: (a -> Bool) -> Matrix (Maybe a) -> Matrix (Maybe a)
filterMMaybe predicate (Matrix a) = Matrix [map (maybeFilter predicate) line | line <- a]

filterM :: (a -> Bool) -> Matrix a -> Matrix a
filterM predicate (Matrix a) = Matrix [filter predicate line | line <- a]

instance Functor Matrix where
    fmap :: (a -> b) -> Matrix a -> Matrix b
    fmap func (Matrix a) = Matrix [map func x | x <- a]

instance Applicative Matrix where
    pure :: a -> Matrix a
    pure a = Matrix (repeat $ repeat a)
    (<*>) :: Matrix (a -> b) -> Matrix a -> Matrix b
    (<*>) (Matrix funcs) (Matrix elems) = Matrix (zipWith overlay funcs elems)

overlay :: [t -> a] -> [t] -> [a]
overlay = zipWith ($)

biQuadMatrix :: a -> Matrix a
biQuadMatrix x = Matrix [[x, x], [x, x]]

slice :: Int -> Int -> [a] -> [a]
slice startIndex endIndex = drop startIndex . take (endIndex + 1)

subMatrix :: Int -> Int -> Int -> Int -> Matrix a -> Matrix a
subMatrix startColInd endColInd startLineInd endLineInd (Matrix a) =
    Matrix $ map (slice startColInd endColInd) (slice startLineInd endLineInd a)

takeColumnM :: Int -> Matrix b -> [b]
takeColumnM columnI (Matrix a) = map (!! columnI) a

getAllColumns :: Matrix b -> [[b]]
getAllColumns (Matrix a) = [col | n <- [0 .. (length a - 1)], col <- [takeColumn n a]]
    where
        takeColumn :: Int -> [[b]] -> [b]
        takeColumn columnI = map (!! columnI)

scalarMult :: (Num a) => [a] -> [a] -> a
scalarMult v1 v2 = if length v1 /= length v2 then error "length of vectors are not equal" else sum $ zipWith (*) v1 v2

multiplyM :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiplyM (Matrix a) (Matrix b) = Matrix [map scalarMult (getAllColumns (Matrix b)) <*> [line] | line <- a]

-- unionMatrix :: [Matrix a] -> Matrix a
-- unionMatrix [] = Matrix [[]]
-- unionMatrix [Matrix x] = Matrix x
-- unionMatrix ((Matrix x) : ((Matrix y) : xs)) = unionMatrix (Matrix (union2 x y) : xs) where
--     union2 (x:xs) (y:ys) = (x ++ y) : union2 xs ys
--     union2 _ _ = []

-- concatMatrix :: [Matrix a] -> Matrix a
-- concatMatrix [] = Matrix [[]]
-- concatMatrix ((Matrix x) : ((Matrix y) : xs)) = concatMatrix (Matrix (x ++ y) : xs)
-- concatMatrix [Matrix x] = Matrix x

-- Видимо для монады так нельзя. Ну или как-то переопределять <*>, чтобы он тоже ветвился
-- m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))) не выполняется
-- instance Monad Matrix where
--     return :: a -> Matrix a
--     return = pure
--     (>>=) :: Matrix a -> (a -> Matrix b) -> Matrix b
--     (Matrix x) >>= ka = concatMatrix secondStep where
--         secondStep = [unionMatrix a | a <- firstStep] where
--             firstStep = [map ka line | line <- x]
