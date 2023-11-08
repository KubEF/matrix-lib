module ParseMTX where

import GenTH
import Helpers
import System.IO

data MtxSparseFormat a = Mtx {values :: [(Int, Int, a)], linesCount :: Int, columnCount :: Int} deriving (Show)

readSpecDouble :: String -> Double
readSpecDouble a
    | head a == '.' = read ('0' : a)
    | head a == '-' && (a !! 1) == '.' = read ("-0" ++ tail a)
    | otherwise = read a

generateVoid :: (Num a) => (Int, Int, c) -> [[a]]
generateVoid (mLines, columns, _) = replicate mLines (replicate columns 0)

fillInfo :: [(Int, Int, c)] -> [[c]] -> [[c]]
fillInfo info voidList = zipWith forLines voidList [1 .. length voidList]
    where
        findThreeByFirst element = filter (\(f, _, _) -> f == element)
        findThreeBySecond element = filter (\(_, s, _) -> s == element)
        forLines line indexL = if not (null threesL) then zipWith forColumn line [1 .. length line] else line
            where
                threesL = findThreeByFirst indexL info
                forColumn column indexC = if not (null threeC) && (indexC == takeSnd (head threeC)) then takeThrd (head threeC) else column
                    where
                        threeC = findThreeBySecond indexC threesL

readFuncToMatrix :: String -> IO [[Double]]
readFuncToMatrix filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let singleWords = lines contents
        parsedWords = map (listToTuple3 . words) (filter (notElem '%') singleWords)
        numbers = map (\(i, j, val) -> (read i :: Int, read j :: Int, readSpecDouble val)) parsedWords
        metaInfo = head numbers
        info = tail numbers
        voidLists = generateVoid metaInfo
        list = fillInfo info voidLists
    -- hClose handle
    return list

readFuncToMtxFormat :: FilePath -> IO (MtxSparseFormat Double)
readFuncToMtxFormat filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let singleWords = lines contents
        parsedWords = map (listToTuple3 . words) (filter (notElem '%') singleWords)
        numbers = map (\(i, j, val) -> (read i :: Int, read j :: Int, readSpecDouble val)) parsedWords
        metaInfo = head numbers
        info = tail numbers
        mtx = Mtx info (takeFst metaInfo) (takeSnd metaInfo)
    -- hClose handle
    return mtx