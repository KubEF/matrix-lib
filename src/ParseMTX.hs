module ParseMTX where

import System.IO

readSpecDouble :: String -> Double
readSpecDouble a
    | head a == '.' = read ('0' : a)
    | head a == '-' && (a !! 1) == '.' = read ("-0" ++ tail a)
    | otherwise = read a

toTuple3 :: [c] -> (c, c, c)
toTuple3 list = (head list, list !! 1, last list)

generateVoid :: (Num a) => (Int, Int, c) -> [[a]]
generateVoid (mLines, columns, _) = replicate mLines (replicate columns 0)

mySnd :: (a, b, c) -> b
mySnd (_, b, _) = b

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

fillInfo :: (Num c) => [(Int, Int, c)] -> [[c]] -> [[c]]
fillInfo info voidList = zipWith forLines voidList [1 .. length voidList]
    where
        findThreeByFirst element = filter (\(f, _, _) -> f == element)
        findThreeBySecond element = filter (\(_, s, _) -> s == element)
        forLines line indexL = if not (null threesL) then zipWith forColumn line [1 .. length line] else line
            where
                threesL = findThreeByFirst indexL info
                forColumn column indexC = if not (null threeC) && (indexC == mySnd (head threeC)) then column + thrd (head threeC) else column
                    where
                        threeC = findThreeBySecond indexC threesL

readFunc :: String -> IO [[Double]]
readFunc filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let singleWords = lines contents
        parsedWords = map (toTuple3 . words) (filter (notElem '%') singleWords)
        numbers = map (\(i, j, val) -> (read i :: Int, read j :: Int, readSpecDouble val)) parsedWords
        metaInfo = head numbers
        info = tail numbers
        voidLists = generateVoid metaInfo
        list = fillInfo info voidLists
    -- hClose handle
    return list