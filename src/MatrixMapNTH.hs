{-# LANGUAGE TemplateHaskell #-}

module MatrixMapNTH(genMaps) where

import Control.Monad (replicateM)
import Language.Haskell.TH

mapN :: Int -> Q Dec
mapN n
    | n >= 1 = funD name [cl1]
    | otherwise = fail "mapN: argument n may not be <= 0."
    where
        name = mkName $ "map" ++ show n
        cl1 = do
            f <- newName "f"
            xs <- replicateM n (newName "x")
            let argsPatts = varP f : (varP <$> xs)
            let road =
                    foldl
                        (\acc matrix -> [|$acc <*> $(varE matrix)|])
                        [|fmap $(varE f) $(varE $ head xs)|]
                        (tail xs)
            clause argsPatts (normalB road) []

mapNSignature :: Int -> Q Dec
mapNSignature n
    | n > 1 = sigD name sinHelp
    | otherwise = fail "mapN: argument n may not be <= 1."
    where
        name = mkName $ "map" ++ show n
        sinHelp = do
            xsNames <- replicateM n (newName "x")
            let xs = varT <$> xsNames
            resultName <- newName "r"
            let result = varT resultName
            applFName <- newName "f"
            let applF = varT applFName
            let sigFuncNArgs = makeFunc (xs ++ [result])
            let mapNArgs = sigFuncNArgs : [appT applF x | x <- xs] ++ [appT applF result]
            forallT [] (sequence [appT (conT ''Applicative) applF]) (makeFunc mapNArgs)

genMaps :: Int -> Q [Dec]
genMaps n = do
    masDec <- mapM (\x -> sequence [mapNSignature x, mapN x]) [2 .. n]
    return $ concat masDec

-- sumOfKMatrix k 
--     | k < 2 = fail "You can`t sum less then 2 matrix"
--     | otherwise = funD name [cl1]
--     where
--         name = mkName $ "sumOf" ++show k ++ "Matrix"
--         cl1 = do
--             clause [_] (_) []


makeFunc :: (Quote m) => [m Type] -> m Type
makeFunc = foldr1 (appT . appT arrowT)
-- makeFunc [varT a1, varT a2, varT a3] = a1 -> a2 -> a3