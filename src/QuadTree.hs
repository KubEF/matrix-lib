{-# OPTIONS_GHC -Wno-partial-fields #-}

module QuadTree where

import Data.Maybe
import Matrix
import ParseMTX

data Unit a = Unit {value :: a, size :: Int} deriving (Show)

instance (Eq a) => Eq (Unit a) where
    (==) :: Unit a -> Unit a -> Bool
    (==) (Unit v1 s1) (Unit v2 s2) = v1 == v2 && s1 == s2

-- instance (Show a) => Show (Unit a) where
--     show :: Unit a -> String
--     show (Unit value size) = unlines $ replicate size (show $ replicate size value)

data QuadTree a = None | Leaf (Unit a) | Node {nw :: QuadTree a, ne :: QuadTree a, sw :: QuadTree a, se :: QuadTree a} deriving (Show)

countOfLeafs :: QuadTree a2 -> Int
countOfLeafs qt = case qt of
    None -> 0
    (Node nw ne sw se) -> countOfLeafs nw + countOfLeafs ne + countOfLeafs sw + countOfLeafs se
    Leaf _ -> 1

binFunc :: (Integral (Unit t), Eq t) => (t -> t -> t) -> QuadTree t -> QuadTree t -> QuadTree t
binFunc f q1 q2 = case (q1, q2) of
    (None, None) -> None
    (None, Leaf un) ->
        if size un == 1
            then Leaf un
            else
                Node
                    (binFunc f None (Leaf $ Unit (value un) (size un) `div` 2))
                    (Leaf $ Unit (value un) (size un) `div` 2)
                    (Leaf $ Unit (value un) (size un) `div` 2)
                    (Leaf $ Unit (value un) (size un) `div` 2)
    (Leaf un, None) ->
        if size un == 1
            then Leaf un
            else
                Node
                    (binFunc f (Leaf $ Unit (value un) (size un) `div` 2) None)
                    (Leaf $ Unit (value un) (size un) `div` 2)
                    (Leaf $ Unit (value un) (size un) `div` 2)
                    (Leaf $ Unit (value un) (size un) `div` 2)
    (None, node@(Node{})) -> node
    (node@(Node{}), None) -> node
    (l1@(Leaf (Unit v1 s1)), l2@(Leaf (Unit v2 s2))) ->
        if s1 == s2
            then Leaf (Unit (v1 `f` v2) s1)
            else
                if s1 > s2
                    then
                        Node
                            (binFunc f (Leaf $ Unit v1 (s1 `div` 2)) l2)
                            (Leaf $ Unit v1 (s1 `div` 2))
                            (Leaf $ Unit v1 (s1 `div` 2))
                            (Leaf $ Unit v1 (s1 `div` 2))
                    else
                        Node
                            (binFunc f l1 (Leaf $ Unit v2 (s2 `div` 2)))
                            (Leaf $ Unit v2 (s2 `div` 2))
                            (Leaf $ Unit v2 (s2 `div` 2))
                            (Leaf $ Unit v2 (s2 `div` 2))
    (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) ->
        Node
            (binFunc f nw1 nw2)
            (binFunc f ne1 ne2)
            (binFunc f sw1 sw2)
            (binFunc f se1 se2)
    (Node nw ne sw se, l@(Leaf _)) -> Node (binFunc f nw l) ne sw se
    (l@(Leaf _), Node nw ne sw se) -> Node (binFunc f nw l) ne sw se

-- instance (Show a) => Show (QuadTree a) where
--     show (Node nw ne sw se) = show nw ++ " | " ++ show ne ++ "\n" ++ show sw ++ " | " ++ show se
--     show (Unit some) = init $ show some

roundUp :: (RealFrac a1, Enum a1, Enum a2) => a1 -> a2
roundUp x = if x > toEnum (round x) then toEnum $ round x + 1 else toEnum $ round x

-- upper matrix to quad with Nothing at nonexisitings places
toQuad :: Matrix (Maybe a) -> Matrix (Maybe a)
toQuad matrix@(Matrix a)
    | length a == 2 * length matrix = matrix
    | otherwise =
        Matrix $
            [line ++ replicate (size - length line) Nothing | line <- a]
                ++ replicate (size - length a) (replicate size Nothing)
    where
        size = 2 ^ roundUp (logBase 2 (toEnum $ max (length a) (length $ head a)))

-- partial matrix to 4
{-
__________
    |     |
 NW |  NE |
__________|
    |     |
 SW |  SE |
__________|
-}
tablePartition :: Matrix a -> [Matrix a]
tablePartition (Matrix b) =
    [ Matrix $ map (take sizeColumns) (take sizeLines b)
    , Matrix $ map (drop sizeColumns) (take sizeLines b)
    , Matrix $ map (take sizeColumns) (drop sizeLines b)
    , Matrix $ map (drop sizeColumns) (drop sizeLines b)
    ]
    where
        sizeLines = roundUp (toRational (length b) / 2)
        sizeColumns = roundUp (toRational (length $ head b) / 2)

-- roundUp x y = x `div` y + x `mod` y

toQuadTreeFromTableMatrix :: (Eq a) => Matrix (Maybe a) -> QuadTree (Maybe a)
toQuadTreeFromTableMatrix m
    | null matrix = None
    | equals (head ll) && equals ll = if isNothing ((head . head) ll) then None else Leaf (toUnit matrix)
    | otherwise =
        Node
            (toQuadTreeFromTableMatrix $ head quads)
            (toQuadTreeFromTableMatrix $ quads !! 1)
            (toQuadTreeFromTableMatrix $ quads !! 2)
            (toQuadTreeFromTableMatrix $ quads !! 3)
    where
        matrix@(Matrix ll) = toQuad m
        equals l = all (h ==) l where h = head l
        quads = tablePartition matrix
        toUnit (Matrix a) = Unit ((head . head) a) (length a)

reduce :: (Eq a) => QuadTree a -> QuadTree a
reduce quadTreeNode = case quadTreeNode of
    (Node None None None None) -> None
    (Node (Leaf nw) (Leaf ne) (Leaf sw) (Leaf se)) ->
        if nw == ne && ne == sw && sw == se && se == nw
            then Leaf $ Unit (value nw) (2 * size nw)
            else quadTreeNode
    _ -> quadTreeNode

mtxFormatPartition :: MtxSparseFormat a -> (MtxSparseFormat a, MtxSparseFormat a, MtxSparseFormat a, MtxSparseFormat a)
mtxFormatPartition mtx@(Mtx values rows columns)
    | columns == 0 || rows == 0 = (mtx, mtx, mtx, mtx)
    | otherwise = (Mtx nw halfRows halfColumns, Mtx ne halfRows halfColumns, Mtx sw halfRows halfColumns, Mtx se halfRows halfColumns)
    where
        halfRows = roundUp (toRational rows / 2)
        halfColumns = roundUp (toRational columns / 2)
        inner lst nw' ne' sw' se' = case lst of
            [] -> (nw', ne', sw', se')
            ((i, j, value) : tl) ->
                if i <= halfRows && j <= halfColumns
                    then inner tl ((i, j, value) : nw') ne' sw' se'
                    else
                        if i <= halfRows && j > halfColumns
                            then inner tl nw' ((i, j - halfColumns, value) : ne') sw' se'
                            else
                                if i > halfRows && j <= halfColumns
                                    then inner tl nw' ne' ((i - halfRows, j, value) : sw') se'
                                    else inner tl nw' ne' sw' ((i - halfRows, j - halfRows, value) : se')
        (nw, ne, sw, se) = inner values [] [] [] []

toQuadTreeFromMtxFormat :: (Eq a) => MtxSparseFormat a -> QuadTree a
toQuadTreeFromMtxFormat (Mtx values rows columns)
    | rows == 0 && columns == 0 = None
    | rows == 1 && columns == 1 && not (null values) = Leaf $ Unit (takeThrd $ head values) 1
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
                Leaf $ Unit (takeThrd $ head values') 1
            | null values' = None
            | otherwise = reduce $ Node (inner nw) (inner ne) (inner sw) (inner se)
            where
                (nw, ne, sw, se) = mtxFormatPartition mtx'

readAndPrintMatrix :: String -> IO ()
readAndPrintMatrix path = do
    m1 <- readFuncToMatrix path
    -- let matrix = Matrix m1
    --     quadMatrix = toQuadTreeFromTableMatrix (fmap Just matrix)
    m <- readFuncToMtxFormat path
    let quadMatrix = toQuadTreeFromMtxFormat m
    print $ countOfLeafs quadMatrix
    print quadMatrix
