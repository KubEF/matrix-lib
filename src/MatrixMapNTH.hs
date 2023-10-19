{-# LANGUAGE TemplateHaskell #-}

module MatrixMapNTH where

import Control.Monad (forM, replicateM)
import Language.Haskell.TH (
    Dec,
    Q,
    Quote (newName),
    clause,
    funD,
    mkName,
    normalB,
    varE,
    varP,
 )

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

genMaps :: Int -> Q [Dec]
genMaps n = forM [1 .. n] mapN
