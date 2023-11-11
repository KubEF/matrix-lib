module Main where

import GenQuadTreeTH
import GenTH ()
import Helpers
import Matrix
import ParseMTX (readFuncToMtxFormat)
import QuadTree (QuadTree, map2QuadTree, toQuadTreeFromTableMatrix)
import System.Exit as Exit
import Test.HUnit (
    Counts (failures),
    Test (..),
    assertEqual,
    runTestTT,
 )

add2QT :: QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double)
add2QT = map2QuadTree maybeAdd

zipWithSum4 :: QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double) -> QuadTree (Maybe Double)
zipWithSum4 quadTree1 quadTree2 quadTree3 quadTree4 =
    quadTree1 `add2QT` quadTree2 `add2QT` quadTree3 `add2QT` quadTree4

testFunc :: (Show a, Eq a) => (a -> a -> a) -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> String -> Test
testFunc func quadTree1 quadTree2 quadTree3 quadTree4 expectedRes text =
    TestCase $ assertEqual text expectedRes actRes
    where
        actRes = zipWithBinFunc4 func quadTree1 quadTree2 quadTree3 quadTree4

wrapToMaybeQuadTree :: (Eq a) => Matrix a -> QuadTree (Maybe a)
wrapToMaybeQuadTree matrix =
    toQuadTreeFromTableMatrix $ sequenceA $ Just matrix

main :: IO ()
main = do
    quadTree1 <- readFuncToMtxFormat "test/my1.mtx"
    quadTree2 <- readFuncToMtxFormat "test/my2.mtx"
    let expectedResAdd =
            wrapToMaybeQuadTree $
                Matrix
                    [ [4.0, 12, 0, 0]
                    , [0, 16, 0, 0]
                    , [0, 0, 0, 0]
                    , [8, 0, 0, 0]
                    ]
        expectedResSub =
            wrapToMaybeQuadTree $
                Matrix $
                    replicate 4 (replicate 4 0.0)
        testSubst = testFunc maybeSubtract quadTree2 quadTree1 quadTree1 quadTree1 expectedResSub "subtract to zero matrix"
        testAdd = testFunc maybeAdd quadTree1 quadTree1 quadTree1 quadTree1 expectedResAdd "adds four equals matrix"
        testEquals = testFunc maybeAdd quadTree1 quadTree2 quadTree1 quadTree2 (zipWithSum4 quadTree1 quadTree2 quadTree1 quadTree2) "equals of TH realization and map2 realization"
        tests = TestList [testAdd, testSubst, testEquals]
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

-- main = quickCheck prop_revapp