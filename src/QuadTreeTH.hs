{-# LANGUAGE TemplateHaskell #-}

module QuadTreeTH where

import Data.List (zip4, zipWith4)
import GenTH
import Helpers
import Language.Haskell.TH
import QuadTree

makeFunc :: (Quote m) => [m Type] -> m Type
makeFunc = foldr1 (appT . appT arrowT)

-- returns list of numbered regular names, for example:
-- numberedNames "t" 4 == [t1, t2, t3, t4]
numberedNames :: String -> Int -> [Name]
numberedNames x k = [mkName $ x ++ show n | n <- [1 .. k]]

-- wraps list of names into expression variables
namesToExp :: (Quote m) => [Name] -> [m Exp]
namesToExp = map varE

-- wraps list of names into pattern variables
namesToPat :: (Quote m) => [Name] -> [m Pat]
namesToPat = map varP

-- wraps list of names into type variables
namesToType :: (Quote m) => [Name] -> [m Type]
namesToType = map varT

-- generate match of pattern variables by list, for example:
-- matchTupleByList body [t1, t2, t3] = (t1, t2, t3) -> body
-- where body is body of function that you want
matchTupleByList :: (Quote m) => m Body -> [m Pat] -> m Match
matchTupleByList body list = match (tupP list) body []

-- generate pattern of Leaf without args and bind it with given name, for example
-- leafWithoutArgs l1 = l1@(Leaf{})
leafWithoutArgs :: (Quote m) => Name -> m Pat
leafWithoutArgs name = asP name [p|Leaf{}|]

-- generate pattern of Leaf with given patterns args, for example
-- leafWithArgs v1 s1 = (Leaf v1 s1)
-- note that v1 and s1 are not names, so you have to wrap wishing names into pattern variables
leafWithArgsPat :: (Quote m) => (m Pat, m Pat) -> m Pat
leafWithArgsPat (v, s) = [p|Leaf $v $s|]

leafWithArgsExp :: (Quote m) => (m Exp, m Exp) -> m Exp
leafWithArgsExp (v, s) = [|Leaf $v $s|]

-- generate pattern of Node with given patterns args, for example
-- nodeWithArgs (nw1, ne1, sw1, se1) = Node nw1 ne1 sw1 se1
-- note that nw1 , ne1, sw1 ,se1 are not names, so you have to wrap it into pattern variables
-- also note that this is fully curried function. So you can zip it with zip4 before using
nodeWithArgsPat :: (Quote m) => (m Pat, m Pat, m Pat, m Pat) -> m Pat
nodeWithArgsPat (nw, ne, sw, se) = [p|Node $nw $ne $sw $se|]

nodeWithArgsExp :: (Quote m) => (m Exp, m Exp, m Exp, m Exp) -> m Exp
nodeWithArgsExp (nw, ne, sw, se) = [|Node $nw $ne $sw $se|]

-- generate 'case (x1, ..., xn) of matches' expression by given expression vars, for example
-- caseArgs [t1, t2, t3, t4] matches = case (t1, t2, t3, t4) of matches
-- where matches is pattern matching with wishing bodies
caseArgs :: (Quote m) => [m Exp] -> [m Match] -> m Exp
caseArgs varsList = caseE (tupE varsList)

-- generate 'if s1 == s2 == ... == sn then thenBody else elseBody' expression
ite :: (Quote m) => m Exp -> m Exp -> [m Exp] -> m Exp
ite thenBody elseBody sizesList =
    [|
        if $(equalsCondition)
            then $thenBody
            else $elseBody
        |]
    where
        equalsCondition =
            foldl
                (\acc x -> [|$(acc) && $(head sizesList) == $x|])
                [|$(head sizesList) == $(head $ tail sizesList)|]
                (tail $ tail sizesList)

-- generate zip by binary function expression, for example
-- zipArgsByBin (+) [x1, x2, x3, x4] = x1 + x2 + x3 + x4
zipArgsByBin :: (Quote m) => m Exp -> [m Exp] -> m Exp
zipArgsByBin func = foldl1 (\acc x -> infixE (Just acc) func (Just x))

whereSizeDiv2 :: (Quote m) => Name -> m Exp -> m Dec
whereSizeDiv2 s s1 = valD (varP s) (normalB [|$s1 `div` 2|]) []

recAppl :: (Quote m) => m Exp -> m Exp -> [m Exp] -> m Exp
recAppl myF f = foldl (\acc x -> [|$acc $x|]) [|$myF $f|]

anagramToExp :: (Quote m) => [[m Exp]] -> [m Exp]
anagramToExp = map (\x -> if length x == 2 then leafWithArgsExp (listToTuple2 x) else nodeWithArgsExp (listToTuple4 x))

anagramToPat :: [[Name]] -> [Q Pat]
anagramToPat = map (\x -> if length x == 2 then leafWithArgsPat (listToTuple2 $ namesToPat x) else nodeWithArgsPat (listToTuple4 $ namesToPat x))

genBinFunc :: Int -> Q [Dec]
genBinFunc k = sequenceA [sigD name typ, funD name [cl1]]
    where
        name = mkName $ "zipWithBinFunc" ++ show k
        vNames = numberedNames "v" k
        sNames = numberedNames "s" k
        nwNames = numberedNames "nw" k
        neNames = numberedNames "ne" k
        swNames = numberedNames "sw" k
        seNames = numberedNames "se" k
        qNames = numberedNames "q" k
        fName = mkName "f"
        sExp = varE $ mkName "s"
        sName = mkName "s"
        cl1 = do
            -- [(Leaf v1 s1), ..., (Leaf vk sk)] :: Pat
            let
                leafsPat =
                    zipWith (curry leafWithArgsPat) (namesToPat vNames) (namesToPat sNames)
                -- [Node nw1 ne1 sw1 se1, ..., Node nwk nek swk sek] :: Pat
                nodesPat =
                    map
                        nodeWithArgsPat
                        ( zip4
                            (namesToPat nwNames)
                            (namesToPat neNames)
                            (namesToPat swNames)
                            (namesToPat seNames)
                        )
                -- f q1 ... qk :: Pat
                mainPat = namesToPat (fName : qNames)
                -- anagrams [[v1, s1], .., [vn, sn]] [[nw1, ne1, sw1, se1], .., [nw(k-n), ne(k-n), sw(k-n), se(k-n)]]
                anagramsNLeafsName n =
                    anagrams
                        (take n (zipWith (\x y -> [x, y]) vNames sNames))
                        (take (k - n) (zipWith4 (\x y z w -> [x, y, z, w]) nwNames neNames swNames seNames))

                anagramNLeafsPat = tupP . anagramToPat
            -- if pattern have at least 1 Node and at least 1 Leaf, then generate body. Example with n Leafs and accordingly (k - n) Nodes
            {-
            reduce $
                Node
                    (zipWithBinFuncK f nw1 .. nw(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (zipWithBinFuncK f ne1 .. ne(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (zipWithBinFuncK f sw1 .. sw(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (zipWithBinFuncK f se1 .. se(k-n) (Leaf v1 s) ... (Leaf vn s )
            -}
            -- s is common size of leafs that equals s1 div 2 (s1 == s2 == ... = sn)
            -- ord of leafs and nodes are important
            let bodyToCenter anagram =
                    [|
                        reduce $
                            Node
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ head x) anagram)
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 1) anagram)
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 2) anagram)
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 3) anagram)
                                 )
                        |]
            -- generate match at least 1 Nodes and at least one Leafs. anagrams are all combinations of this pattern, right body and block
            {-
            where s = s1 div 2
            -}
            let matchesToCenter anagram =
                    match
                        (anagramNLeafsPat anagram)
                        (normalB $ bodyToCenter anagram)
                        [whereSizeDiv2 sName (varE $ head sNames)]
                        :: Q Match
                -- generate all cases with that match
                listOfCenterMatches = concat [map matchesToCenter (anagramsNLeafsName n) | n <- [1 .. k - 1]]
                elseBodyExp = [|error "different size of leafs"|]
                thenBodyExp = [|Leaf ($(zipArgsByBin (varE fName) (namesToExp vNames))) ($(varE $ head sNames))|]
                bodyAllNodes =
                    [|
                        reduce $
                            Node
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name) $(varE fName)|] nwNames)
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name) $(varE fName)|] neNames)
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name) $(varE fName)|] swNames)
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name) $(varE fName)|] seNames)
                        |]
                matchAllNodes =
                    match
                        (tupP nodesPat)
                        (normalB bodyAllNodes)
                        []
                matchAllLeafs =
                    match
                        (tupP leafsPat)
                        (normalB $ ite thenBodyExp elseBodyExp (namesToExp sNames))
                        []
                mainBody = caseArgs (namesToExp qNames) (matchAllNodes : matchAllLeafs : listOfCenterMatches)
            clause mainPat (normalB mainBody) []
        typ = do
            let a = varT $ mkName "a"
            let functionSig = makeFunc (replicate 3 a)
                quadTreesList = replicate (k + 1) [t|QuadTree $a|]
            forallT [] (sequence [appT (conT ''Eq) a]) (makeFunc (functionSig : quadTreesList)) :: Q Type

aplNFuncs :: (Quote m) => m Exp -> [m Exp] -> [m Exp] -> m Exp
aplNFuncs name funcs anagram = foldl (\acc x -> [|$acc $x|]) [|$name|] (funcs ++ anagram)

mapArgsByFuncs :: (Quote m) => [m Exp] -> [m Exp] -> m Exp
mapArgsByFuncs funcs args = foldl (\acc x -> infixE (Just acc) (snd x) (Just $ fst x)) (head args) (zip (tail args) funcs)

genBinKFunc :: Int -> Q [Dec]
genBinKFunc k = sequenceA [sigD name typ, funD name [cl1]]
    where
        name = mkName $ "zipWith" ++ show k ++ "Funcs"
        vNames = numberedNames "v" k
        sNames = numberedNames "s" k
        nwNames = numberedNames "nw" k
        neNames = numberedNames "ne" k
        swNames = numberedNames "sw" k
        seNames = numberedNames "se" k
        qNames = numberedNames "q" k
        fNames = numberedNames "f" (k - 1)
        sExp = varE $ mkName "s"
        sName = mkName "s"
        cl1 = do
            -- [(Leaf v1 s1), ..., (Leaf vk sk)] :: Pat
            let
                leafsPat =
                    zipWith (curry leafWithArgsPat) (namesToPat vNames) (namesToPat sNames)
                -- [Node nw1 ne1 sw1 se1, ..., Node nwk nek swk sek] :: Pat
                nodesPat =
                    map
                        nodeWithArgsPat
                        ( zip4
                            (namesToPat nwNames)
                            (namesToPat neNames)
                            (namesToPat swNames)
                            (namesToPat seNames)
                        )
                -- f1 ... f(k-1) q1 ... qk :: Pat
                mainPat = namesToPat (fNames ++ qNames)
                -- anagrams [[v1, s1], .., [vn, sn]] [[nw1, ne1, sw1, se1], .., [nw(k-n), ne(k-n), sw(k-n), se(k-n)]]
                anagramsNLeafsName n =
                    anagrams
                        (take n (zipWith (\x y -> [x, y]) vNames sNames))
                        (take (k - n) (zipWith4 (\x y z w -> [x, y, z, w]) nwNames neNames swNames seNames))

                anagramNLeafsPat = tupP . anagramToPat
            -- if pattern have at least 1 Node and at least 1 Leaf, then generate body. Example with n Leafs and accordingly (k - n) Nodes
            {-
            reduce $
                Node
                    (quadTreeMapK f1 ... f(k-1) nw1 .. nw(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (quadTreeMapK f1 ... f(k-1) ne1 .. ne(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (quadTreeMapK f1 ... f(k-1) sw1 .. sw(k-n) (Leaf v1 s) ... (Leaf vn s )
                    (quadTreeMapK f1 ... f(k-1) se1 .. se(k-n) (Leaf v1 s) ... (Leaf vn s )
            -}
            -- s is common size of leafs that equals s1 div 2 (s1 == s2 == ... = sn)
            -- ord of leafs and nodes are important
            let bodyToCenter anagram =
                    [|
                        reduce $
                            Node
                                $( aplNFuncs
                                    (varE name)
                                    (namesToExp fNames)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ head x) anagram)
                                 )
                                $( aplNFuncs
                                    (varE name)
                                    (namesToExp fNames)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 1) anagram)
                                 )
                                $( aplNFuncs
                                    (varE name)
                                    (namesToExp fNames)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 2) anagram)
                                 )
                                $( aplNFuncs
                                    (varE name)
                                    (namesToExp fNames)
                                    (map (\x -> if length x == 2 then [|Leaf $(varE $ head x) $(sExp)|] else varE $ x !! 3) anagram)
                                 )
                        |]
            -- generate match at least 1 Nodes and at least one Leafs. anagrams are all combinations of this pattern, right body and block
            {-
            where s = s1 div 2
            -}
            let matchesToCenter anagram =
                    match
                        (anagramNLeafsPat anagram)
                        (normalB $ bodyToCenter anagram)
                        [whereSizeDiv2 sName (varE $ head sNames)]
                        :: Q Match
                -- generate all cases with that match
                listOfCenterMatches = concat [map matchesToCenter (anagramsNLeafsName n) | n <- [1 .. k - 1]]
                elseBodyExp = [|error "different size of leafs"|]
                thenBodyExp = [|Leaf $(mapArgsByFuncs (namesToExp fNames) (namesToExp vNames)) $(varE $ head sNames)|]
                bodyAllNodes =
                    [|
                        reduce $
                            Node
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name)|] (fNames ++ nwNames))
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name)|] (fNames ++ neNames))
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name)|] (fNames ++ swNames))
                                $(foldl (\acc x -> [|$acc $(varE x)|]) [|$(varE name)|] (fNames ++ seNames))
                        |]
                matchAllNodes =
                    match
                        (tupP nodesPat)
                        (normalB bodyAllNodes)
                        []
                matchAllLeafs =
                    match
                        (tupP leafsPat)
                        (normalB $ ite thenBodyExp elseBodyExp (namesToExp sNames))
                        []
                mainBody = caseArgs (namesToExp qNames) (matchAllNodes : matchAllLeafs : listOfCenterMatches)
            clause mainPat (normalB mainBody) []
        typ = do
            let tTypes = namesToType $ numberedNames "t" (2 * k - 2)
                a = varT $ mkName "a"
                functionsSig = map makeFunc (triplePart (tTypes ++ [a])) :: [Q Type]
                quadTreesList =
                    [[t|QuadTree $t|] | n <- lsEvenWithOne (k + 1), let t = varT $ mkName $ "t" ++ show n]
                        ++ [[t|QuadTree $a|]]
            forallT [] (sequence [appT (conT ''Eq) a]) (makeFunc (functionsSig ++ quadTreesList))

genMulWithElement :: Q [Dec]
genMulWithElement =
    [d|
        multiplyWithElementsFunc :: (Eq a, Eq t4) => (t4 -> t4 -> t4) -> (t4 -> t4 -> t4) -> (t4 -> t4 -> a) -> QuadTree.QuadTree t4 -> QuadTree.QuadTree t4 -> QuadTree.QuadTree t4 -> QuadTree.QuadTree a
        multiplyWithElementsFunc mulFunc addFunc elementsFunc q1 q2 q3 = case (q1, q2) of
            (Leaf v1 s1, Leaf v2 s2) ->
                let mul = v1 `mulFunc` v2
                in  if s1 == s2
                        then map2QuadTree elementsFunc (Leaf (scalarMul addFunc mul s1) s1) q3
                        else error "incorrect input: you cannot get leafs with different sizes"
            (Node nw1 ne1 sw1 se1, Node nw2 ne2 sw2 se2) ->
                reduce $
                    Node
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul nw1 nw2) (innerMul ne1 sw2) q3One)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul nw1 ne2) (innerMul ne1 se2) q3Two)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul sw1 nw2) (innerMul se1 sw2) q3Three)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul sw1 ne2) (innerMul se1 se2) q3Four)
            (Leaf v s1, Node nw ne sw se) ->
                reduce $
                    Node
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul l nw) (innerMul l sw) q3One)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul l ne) (innerMul l se) q3Two)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul l nw) (innerMul l sw) q3Three)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul l ne) (innerMul l se) q3Four)
                where
                    s = s1 `div` 2
                    l = Leaf v s
            (Node nw ne sw se, Leaf v s1) ->
                reduce $
                    Node
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul nw l) (innerMul sw l) q3One)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul ne l) (innerMul se l) q3Two)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul nw l) (innerMul sw l) q3Three)
                        ($(varE $ mkName "zipWith3Funcs") addFunc elementsFunc (innerMul ne l) (innerMul se l) q3Four)
                where
                    s = s1 `div` 2
                    l = Leaf v s
            where
                innerMul = multiplyQT mulFunc addFunc
                (q3One, q3Two, q3Three, q3Four) =
                    case q3 of
                        Node nw3 ne3 sw3 se3 -> (nw3, ne3, sw3, se3)
                        Leaf v3 s3 -> (Leaf v3 s, Leaf v3 s, Leaf v3 s, Leaf v3 s)
                            where
                                s = s3 `div` 2
        |]
