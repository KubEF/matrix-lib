{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module MatrixMapNTH where

import Control.Monad (replicateM)
import Language.Haskell.TH
import ListTH
import Matrix

$(generateZipWithBinTok 10)
$(generateZipWithTok 10)

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

-- makeFunc [varT a1, varT a2, varT a3] = a1 -> a2 -> a3

genThroughFuncMatrix :: Int -> Q [Dec]
genThroughFuncMatrix k
    | k < 1 = fail "can not manipulate less then 1 matrix"
    | otherwise = sequenceA [funD name [cl1]]
    where
        name = mkName $ "trough" ++ show k ++ "Matrix"
        cl1 = do
            xList <- replicateM k (newName "x")
            function <- newName "f"
            let pat = (varP function) : [[p|Matrix $(varP x)|] | x <- xList]
            let throughBi = mkName $ "zipWithBin" ++ show k
            let through = mkName $ "myZipWith" ++ show k
            let body = foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE through) ($(varE throughBi) $(varE function))|] xList :: Q Exp
            clause pat (normalB [|Matrix $body|]) []
