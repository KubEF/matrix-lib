{-# LANGUAGE DatatypeContexts #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module QuadTree where

import Helpers
import Matrix
import ParseMTX

data QuadTree a
    = Leaf {value :: a, size :: Int}
    | Node {nw :: QuadTree a, ne :: QuadTree a, sw :: QuadTree a, se :: QuadTree a}
    deriving (Show)

instance (Eq a) => Eq (QuadTree a) where
    (==) :: QuadTree a -> QuadTree a -> Bool
    (==) q1 q2 = case (q1, q2) of
        (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) -> (nw1 == nw2) && (ne1 == ne2) && (sw1 == sw2) && (se1 == se2)
        (Leaf v1 s1, Leaf v2 s2) -> (v1 == v2) && (s1 == s2)
        (Node{}, Leaf{}) -> False
        (Leaf{}, Node{}) -> False

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
        sizeLines = roundUp (fromRational $ toRational (length b) / 2)
        sizeColumns = roundUp (fromRational $ toRational (length $ head b) / 2)

-- toQuadTreeFromTableMatrix :: (Eq a) => Matrix (Maybe a) -> QuadTree (Maybe a)
-- toQuadTreeFromTableMatrix m
--     | null matrix = None
--     | equals (head ll) && equals ll = if isNothing ((head . head) ll) then None else Leaf (toUnit matrix)
--     | otherwise =
--         Node
--             (toQuadTreeFromTableMatrix $ head quads)
--             (toQuadTreeFromTableMatrix $ quads !! 1)
--             (toQuadTreeFromTableMatrix $ quads !! 2)
--             (toQuadTreeFromTableMatrix $ quads !! 3)
--     where
--         matrix@(Matrix ll) = toQuad m
--         equals l = all (h ==) l where h = head l
--         quads = tablePartition matrix
--         toUnit (Matrix a) = Unit ((head . head) a) (length a)

reduce :: (Eq a) => QuadTree a -> QuadTree a
reduce quadTreeNode = case quadTreeNode of
    (Node nw ne sw se) ->
        if nw == ne && ne == sw && sw == se && se == nw
            then Leaf (value nw) (2 * size nw)
            else quadTreeNode
    _ -> quadTreeNode

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

binFunc :: Eq a => (t -> t -> a) -> QuadTree t -> QuadTree t -> QuadTree a
binFunc f q1 q2 = case (q1, q2) of
    (Leaf v1 s1, Leaf v2 s2) ->
        if s1 == s2
            then Leaf (v1 `f` v2) s1
            else error "different size of leafs"
    (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) ->
        reduce $
            Node
                (binFunc f nw1 nw2)
                (binFunc f ne1 ne2)
                (binFunc f sw1 sw2)
                (binFunc f se1 se2)
    (Node nw ne sw se, Leaf v1 s1) ->
        reduce $
            Node
                (binFunc f nw (Leaf v1 s))
                (binFunc f ne (Leaf v1 s))
                (binFunc f sw (Leaf v1 s))
                (binFunc f se (Leaf v1 s))
            where 
                s = s1 `div` 2
    (Leaf v1 s1, Node nw ne sw se) ->
        reduce $
            Node
                (binFunc f nw (Leaf v1 s))
                (binFunc f ne (Leaf v1 s))
                (binFunc f sw (Leaf v1 s))
                (binFunc f se (Leaf v1 s))
            where s = s1 `div` 2

