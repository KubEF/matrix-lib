{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module QuadTree where

import Control.DeepSeq (NFData)
import GHC.Generics
import Helpers
import Matrix

data QuadTree a
    = Leaf {value :: a, size :: Int}
    | Node {nw :: QuadTree a, ne :: QuadTree a, sw :: QuadTree a, se :: QuadTree a}
    deriving (Show, Generic, NFData)

instance (Eq a) => Eq (QuadTree a) where
    (==) :: QuadTree a -> QuadTree a -> Bool
    (==) q1 q2 = case (q1, q2) of
        (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) -> (nw1 == nw2) && (ne1 == ne2) && (sw1 == sw2) && (se1 == se2)
        (Leaf v1 s1, Leaf v2 s2) -> (v1 == v2) && (s1 == s2)
        (Node{}, Leaf{}) -> False
        (Leaf{}, Node{}) -> False

-- upper matrix to quad with 0 at nonexisitings places
toQuad :: Matrix (Maybe a) -> Matrix (Maybe a)
toQuad matrix@(Matrix a)
    | isPowOf2 $ length matrix = matrix
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

toQuadTreeFromTableMatrix :: (Eq a) => Matrix (Maybe a) -> QuadTree (Maybe a)
toQuadTreeFromTableMatrix m
    | null matrix = error "matrix has 0 size"
    | equals (head ll) && equals ll = toLeaf matrix
    | otherwise =
        reduce $
            Node
                (toQuadTreeFromTableMatrix $ head quads)
                (toQuadTreeFromTableMatrix $ quads !! 1)
                (toQuadTreeFromTableMatrix $ quads !! 2)
                (toQuadTreeFromTableMatrix $ quads !! 3)
    where
        matrix@(Matrix ll) = toQuad m
        equals l = all (h ==) l where h = head l
        quads = tablePartition matrix
        toLeaf (Matrix a) = Leaf ((head . head) a) (length a)

reduce :: (Eq a) => QuadTree a -> QuadTree a
reduce quadTreeNode = case quadTreeNode of
    (Node l1@(Leaf v1 s1) l2@(Leaf{}) l3@(Leaf{}) l4@(Leaf{})) ->
        if l1 == l2 && l2 == l3 && l3 == l4 && l4 == l1
            then Leaf v1 (2 * s1)
            else quadTreeNode
    _ -> quadTreeNode

map2QuadTree :: (Eq a) => (t -> t -> a) -> QuadTree t -> QuadTree t -> QuadTree a
map2QuadTree f q1 q2 = case (q1, q2) of
    (Leaf v1 s1, Leaf v2 s2) ->
        if s1 == s2
            then Leaf (v1 `f` v2) s1
            else error "different size of leafs"
    (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) ->
        reduce $
            Node
                (map2QuadTree f nw1 nw2)
                (map2QuadTree f ne1 ne2)
                (map2QuadTree f sw1 sw2)
                (map2QuadTree f se1 se2)
    (Node nw ne sw se, Leaf v1 s1) ->
        reduce $
            Node
                (map2QuadTree f nw (Leaf v1 s))
                (map2QuadTree f ne (Leaf v1 s))
                (map2QuadTree f sw (Leaf v1 s))
                (map2QuadTree f se (Leaf v1 s))
        where
            s = s1 `div` 2
    (Leaf v1 s1, Node nw ne sw se) ->
        reduce $
            Node
                (map2QuadTree f nw (Leaf v1 s))
                (map2QuadTree f ne (Leaf v1 s))
                (map2QuadTree f sw (Leaf v1 s))
                (map2QuadTree f se (Leaf v1 s))
        where
            s = s1 `div` 2
