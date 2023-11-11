module ParseMTX where

import GenTH
import Helpers
import QuadTree
import System.IO

data MtxSparseFormat a = Mtx {values :: [(Int, Int, a)], linesCount :: Int, columnCount :: Int} deriving (Show)

readSpecDouble :: String -> Double
readSpecDouble a
    | head a == '.' = read ('0' : a)
    | head a == '-' && (a !! 1) == '.' = read ("-0" ++ tail a)
    | head a == 'e' = read ('1' : a)
    | head a == '-' && (a !! 1) == 'e' = read ("-1" ++ tail a)
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

readFuncToMtxFormat :: FilePath -> IO (QuadTree (Maybe Double))
readFuncToMtxFormat filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let singleWords = lines contents
        parsedWords = map (listToTuple3 . words) (filter (notElem '%') singleWords)
        numbers = map (\(i, j, val) -> (read i :: Int, read j :: Int, readSpecDouble val)) parsedWords
        metaInfo = head numbers
        info = tail numbers
        mtx = toQuadTreeFromMtxFormat $ Mtx info (takeFst metaInfo) (takeSnd metaInfo)
    -- hClose handle
    return mtx

mtxFormatPartition :: MtxSparseFormat a -> (MtxSparseFormat a, MtxSparseFormat a, MtxSparseFormat a, MtxSparseFormat a)
mtxFormatPartition mtx@(Mtx values rows columns)
    | columns == 0 || rows == 0 = (mtx, mtx, mtx, mtx)
    | otherwise = (Mtx nw halfRows halfColumns, Mtx ne halfRows halfColumns, Mtx sw halfRows halfColumns, Mtx se halfRows halfColumns)
    where
        halfRows = roundUp $ fromRational (toRational rows / 2)
        halfColumns = roundUp $ fromRational (toRational columns / 2)
        inner lst nw' ne' sw' se' = case lst of
            [] -> (nw', ne', sw', se')
            ((i, j, value) : tl) -> helper i j value tl
            where
                helper i j value tl
                    | i <= halfRows && j <= halfColumns = inner tl ((i, j, value) : nw') ne' sw' se'
                    | i <= halfRows && j > halfColumns = inner tl nw' ((i, j - halfColumns, value) : ne') sw' se'
                    | i > halfRows && j <= halfColumns = inner tl nw' ne' ((i - halfRows, j, value) : sw') se'
                    | otherwise = inner tl nw' ne' sw' ((i - halfRows, j - halfRows, value) : se')
        (nw, ne, sw, se) = inner values [] [] [] []

toQuadTreeFromMtxFormat :: (Eq a) => MtxSparseFormat a -> QuadTree (Maybe a)
toQuadTreeFromMtxFormat (Mtx values rows columns)
    | rows == 0 && columns == 0 = Leaf Nothing 1
    | rows == 1 && columns == 1 && not (null values) = Leaf (Just $ takeThrd $ head values) 1
    | otherwise = inner $ Mtx values powerSize powerSize
    where
        powerSize = 2 ^ roundUp (logBase 2 (toEnum $ max rows columns))
        maxRowIndex = rows - 1
        maxColumnIndex = columns - 1
        inner mtx'@(Mtx values' rows' columns')
            | rows' == 1
                && columns' == 1
                && length values' == 1
                && takeFst (head values') <= maxRowIndex
                && takeSnd (head values') <= maxColumnIndex =
                Leaf (Just $ takeThrd $ head values') 1
            | null values' = Leaf Nothing powerSize'
            | otherwise = reduce $ Node (inner nw) (inner ne) (inner sw) (inner se)
            where
                (nw, ne, sw, se) = mtxFormatPartition mtx'
                powerSize' = 2 ^ roundUp (logBase 2 (toEnum $ max rows' columns'))