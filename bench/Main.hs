module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.List (unfoldr)
import GHC.Word
import GenQuadTreeTH (multiplyWithElementsFunc, zipWithBinFunc4)
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

zipWithSum4
    :: (QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double))
    -> QuadTree (Maybe Double)
zipWithSum4 (quadTree1, quadTree2, quadTree3, quadTree4) =
    quadTree1 `add2QT` quadTree2 `add2QT` quadTree3 `add2QT` quadTree4

uncurriedZipWithBin
    :: (QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double))
    -> QuadTree (Maybe Double)
uncurriedZipWithBin = uncurry4 (zipWithBinFunc4 maybeAdd)

uncurriedConsMulWithElementsFunc :: (Eq a, Num a) => (QuadTree (Maybe a), QuadTree (Maybe a), QuadTree (Maybe a)) -> QuadTree (Maybe a)
uncurriedConsMulWithElementsFunc (q1, q2, q3) = map2QuadTree maybeConcat (multiplyQT maybeMul maybeAdd q1 q2) q3

uncurriedMulWithElementsFunc :: (Eq a, Num a) => (QuadTree (Maybe a), QuadTree (Maybe a), QuadTree (Maybe a)) -> QuadTree (Maybe a)
uncurriedMulWithElementsFunc (q1, q2, q3) = multiplyWithElementsFunc maybeMul maybeAdd maybeConcat q1 q2 q3

main :: IO ()
main = do
    m1 <- readFuncToMtxFormat "bench/matrixes-for-benches/929901.mtx"
    m2 <- readFuncToMtxFormat "bench/matrixes-for-benches/955128.mtx"
    m3 <- readFuncToMtxFormat "bench/matrixes-for-benches/1000000.mtx"
    m4 <- readFuncToMtxFormat "bench/matrixes-for-benches/1000005.mtx"
    let input1 = (m3, m3, m3, m3)
        input2 = (m1, m2, m3, m4)
        input3 = (m1, m4, m1, m3)
        input4 = (m4, m2, m3, m1)
    defaultMain
        [ bgroup
            "comparing of zipWithAdd4 function: sum four equals matrixes 1"
            [ bench "by TH" $ nf uncurriedZipWithBin input1
            , bench "by map2" $ nf zipWithSum4 input1
            ]
        , bgroup
            "comparing of zipWithAdd4 function: sum m1 m2 m3 m4"
            [ bench "by TH" $ nf uncurriedZipWithBin input2
            , bench "by map2" $ nf zipWithSum4 input2
            ]
        , bgroup
            "comparing of zipWithAdd4 function: sum m1 m4 m1 m3"
            [ bench "by TH" $ nf uncurriedZipWithBin input3
            , bench "by map2" $ nf zipWithSum4 input3
            ]
        , bgroup
            "comparing of zipWithAdd4 function: sum m4 m2 m3 m1"
            [ bench "by TH" $ nf uncurriedZipWithBin input4
            , bench "by map2" $ nf zipWithSum4 input4
            ]
        , bgroup
            "comparing of multiply and applying elements function: m1 * m2 `concat` m3"
            [ bench "with zipWith3" $ nf uncurriedMulWithElementsFunc (m1, m2, m3)
            , bench "consistently" $ nf uncurriedConsMulWithElementsFunc (m1, m2, m3)
            ]
        ]
