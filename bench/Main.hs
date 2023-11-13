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

zipWithSum4
    :: (QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double))
    -> QuadTree (Maybe Double)
zipWithSum4 (quadTree1, quadTree2, quadTree3, quadTree4) =
    quadTree1 `add2QT` quadTree2 `add2QT` quadTree3 `add2QT` quadTree4

uncurriedZipWithBin
    :: (QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double), QuadTree (Maybe Double))
    -> QuadTree (Maybe Double)
uncurriedZipWithBin = uncurry4 (zipWithBinFunc4 maybeAdd)


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
    putStrLn ""
    defaultMain
        [ bgroup
            "comparing of zipWithAdd4 function: sum four equals matrixes"
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
        ]
