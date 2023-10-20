{-# LANGUAGE TemplateHaskell #-}

module MatrixMapNTH where

import Control.Monad (forM, replicateM)
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
            if length xsNames > 2
                then do
                    let sigFuncNArgs = makeFunc (xs ++ [result])
                    -- let sigFuncN = appT (appT arrowT sigFuncNArgs) result
                    let mapNArgs = sigFuncNArgs : [appT applF x | x <- xs] ++ [appT applF result]
                    forallT [] (sequence [appT (conT ''Applicative) applF]) (makeFunc mapNArgs)
                else do
                    --             AppT (AppT ArrowT (VarT t1_23)) (AppT (AppT ArrowT (VarT t2_24)) (VarT t3_25))
                    let sigFuncN = appT (appT arrowT (head xs)) (appT (appT arrowT (last xs)) result)
                    let mapNArgs = sigFuncN : [appT applF x | x <- xs] ++ [appT applF result]
                    forallT [] (sequence [appT (conT ''Applicative) applF]) (makeFunc mapNArgs)

-- genMaps :: Int -> Q [Dec]
genMaps :: Int -> Q [Dec]
genMaps n = do
    masDec <- mapM (\x -> sequence [mapNSignature x, mapN x]) [2 .. n]
    return $ concat masDec

-- myFunc :: Q [Dec]
-- myFunc = sequence [sigD name sig, funD name [cl]]
--     where
--         name = mkName "myUniqFunction"
--         argsNames = [mkName $ 't' : show x | x <- [1 .. 4]]
--         sig = do
--             let sigF = makeFunc (varT <$> argsNames)
--             sigF
--         cl = do
--             clause (varP <$> (init argsNames)) (normalB (varE $ head argsNames)) []

-- [
--     SigD
--         <Name>
--         (ForallT [] [AppT (ConT GHC.Base.Applicative) (VarT x_10)]
--         (AppT (AppT ArrowT (VarT x_13)) (AppT (AppT ArrowT (VarT x_13)) (VarT x_13))))

--      AppT (AppT ArrowT (VarT x_10)) (VarT x_10))
--     ]

-- NameF :: Functor f => a -> f b -> f c -> f d
-- SigD
--      <NameF>
--      (ForallT [] [AppT (ConT ''Functor) (VarT f)]
--      (AppT
--          (AppT ArrowT (VarT a))
--          (AppT
--              (AppT ArrowT (AppT (VarT f) (VarT b)))
--              (AppT
--                  (AppT ArrowT (AppT (VarT f) (VarT c)))
--                  (AppT (VarT f) (VarT d))
--               )
--           )
--        )
--  )
--
makeFunc :: (Quote m) => [m Type] -> m Type
makeFunc xs = foldr (\x acc -> appT (appT arrowT x) acc) (appT (appT arrowT (last $ init xs)) (last xs)) (init $ init xs)

-- makeFunc [varT a1, varT a2, varT a3] = a1 -> a2 -> a3