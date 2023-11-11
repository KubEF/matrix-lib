module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.List (unfoldr)
import GHC.Word
import GenQuadTreeTH (zipWithBinFunc4)
import Helpers
import ParseMTX
import QuadTree
import System.Random

pureGen :: StdGen
pureGen = mkStdGen 137

generateRndList :: Int -> [Word64]
generateRndList n = take n (unfoldr (Just . genWord64) pureGen)

add2QT :: QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double)
add2QT = map2QuadTree maybeAdd

zipWithSum4 :: QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double)
zipWithSum4 quadTree1 quadTree2 quadTree3 quadTree4 =
    quadTree1 `add2QT` quadTree2 `add2QT` quadTree3 `add2QT` quadTree4

main :: IO ()
main = do
    putStrLn "no benches here for now"
    m1 <- readFuncToMtxFormat "bench/1138_bus.mtx"
    m2 <- readFuncToMtxFormat "bench/ex31.mtx"
    m3 <- readFuncToMtxFormat "bench/meg4.mtx"
    m4 <- readFuncToMtxFormat "bench/lp_cre_b.mtx"
    m5 <- readFuncToMtxFormat "bench/mc2depi.mtx"
    putStrLn ""
    defaultMain
        [ bgroup
            "comparing of zipWithAdd4 function. Size: 1138"
            [ bench "by TH" $ nf (zipWithBinFunc4 maybeAdd m1 m1 m1) m1
            , bench "by map2 realization" $ nf (zipWithSum4 m1 m1 m1) m1
            ]
        , bgroup
            "comparing of zipWithAdd4 function. Size: 3909"
            [ bench "by TH" $ nf (zipWithBinFunc4 maybeAdd m2 m2 m2) m2
            , bench "by map2 realization" $ nf (zipWithSum4 m2 m2 m2) m2
            ]
        , bgroup
            "comparing of zipWithAdd4 function. Size: 5806"
            [ bench "by TH" $ nf (zipWithBinFunc4 maybeAdd m3 m3 m3) m3
            , bench "by map2 realization" $ nf (zipWithSum4 m3 m3 m3) m3
            ]
        , bgroup
            "comparing of zipWithAdd4 function. Size: 77137"
            [ bench "by TH" $ nf (zipWithBinFunc4 maybeAdd m4 m4 m4) m4
            , bench "by map2 realization" $ nf (zipWithSum4 m4 m4 m4) m4
            ]
        , bgroup
            "comparing of zipWithAdd4 function. Size: 525825"
            [ bench "by TH" $ nf (zipWithBinFunc4 maybeAdd m5 m5 m5) m5
            , bench "by map2 realization" $ nf (zipWithSum4 m5 m5 m5) m5
            ]
        ]
