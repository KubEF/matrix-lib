{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import Matrix (Matrix (Matrix), map2)
import System.Exit as Exit
import Test.HUnit (
    Counts (failures),
    Test (..),
    assertEqual,
    runTestTT,
 )

-- шаблончики, чтобы проверять было проще
list1 :: [[Maybe Integer]]
list1 = [[Just 1, Just 2], [Just 3, Just 4]]

list2 :: [[Maybe Integer]]
list2 = [[Nothing, Just 6], [Just 7, Just 8]]

list3 :: [[Integer]]
list3 = [[9, 10], [11, 12]]

list4 :: [[Integer]]
list4 = [[13, 14], [15, 16]]

m1 :: Matrix (Maybe Integer)
m1 = Matrix list1

m2 :: Matrix (Maybe Integer)
m2 = Matrix list2

m3 :: Matrix Integer
m3 = Matrix list3

m4 :: Matrix Integer
m4 = Matrix list4

m5 :: Matrix Integer
m5 = Matrix [[22, 24], [26, 28]]

testSum :: Test
testSum = TestCase $ assertEqual "should return \n[22, 24]\n[26, 28]" m5 (map2 (+) m3 m4)

testId :: Test
testId = TestCase $ assertEqual "should return \n[9, 10]\n[11,12]" m3 (map2 const m3 m4)

tests :: Test
tests = TestList [TestLabel "testSum" testSum, TestLabel "testId" testId]

-- prop_revapp :: (Eq a) => Matrix a -> Matrix a -> Bool
-- prop_revapp x y = map2 const x y == x

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

-- main = quickCheck prop_revapp