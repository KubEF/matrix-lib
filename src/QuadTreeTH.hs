{-# LANGUAGE TemplateHaskell #-}

module QuadTreeTH where

import Data.List (zip4)
import Helpers (anagrams)
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
leafWithArgs :: (Quote m) => (m Pat, m Pat) -> m Pat
leafWithArgs (v, s) = [p|Leaf $v $s|]

-- generate pattern of Node with given patterns args, for example
-- nodeWithArgs (nw1, ne1, sw1, se1) = Node nw1 ne1 sw1 se1
-- note that nw1 , ne1, sw1 ,se1 are not names, so you have to wrap it into pattern variables
-- also note that this is fully curried function. So you can zip it with zip4 before using
nodeWithArgs :: (Quote m) => (m Pat, m Pat, m Pat, m Pat) -> m Pat
nodeWithArgs (nw, ne, sw, se) = [p|Node $nw $ne $sw $se|]

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

recAppl :: (Quote m) => m Exp -> m Exp -> [m Exp] -> [m Exp] -> m Exp -> m Exp
recAppl myF f ns vs s = foldl (\acc x -> [|$acc $x|]) [|$myF $f|] listOfAll
    where
        leafs = map (\x -> [|Leaf $x $s|]) vs
        listOfAll = ns ++ leafs

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
            let leafsPat =
                    zipWith (curry leafWithArgs) (namesToPat vNames) (namesToPat sNames)
                -- [Node nw1 ne1 sw1 se1, ..., Node nwk nek swk sek] :: Pat
                nodesPat =
                    map
                        nodeWithArgs
                        ( zip4
                            (namesToPat nwNames)
                            (namesToPat neNames)
                            (namesToPat swNames)
                            (namesToPat seNames)
                        )
                -- f q1 ... qk :: Pat
                mainPat = namesToPat (fName : qNames)
                -- anagrams [l1, .., ln] [n1, .., n(k-n)]
                anagramsNLeafs n = map tupP (anagrams (take n leafsPat) (take (k - n) nodesPat))
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
            let bodyToCenter n =
                    [|
                        reduce $
                            Node
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (namesToExp $ take (k - n) nwNames)
                                    (namesToExp $ take n vNames)
                                    sExp
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (namesToExp $ take (k - n) neNames)
                                    (namesToExp $ take n vNames)
                                    sExp
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (namesToExp $ take (k - n) swNames)
                                    (namesToExp $ take n vNames)
                                    sExp
                                 )
                                $( recAppl
                                    (varE name)
                                    (varE fName)
                                    (namesToExp $ take (k - n) seNames)
                                    (namesToExp $ take n vNames)
                                    sExp
                                 )
                        |]
                -- generate match at least 1 Nodes and at least one Leafs. anagrams are all combinations of this pattern, right body and block
                {-
                where s = s1 div 2
                -}
            let matchesToCenter n anagram =
                    match
                        anagram
                        (normalB $ bodyToCenter n)
                        [whereSizeDiv2 sName (varE $ head sNames)]
                        :: Q Match
                -- generate all cases with that match
                listOfCenterMatches = concat [map (matchesToCenter n) (anagramsNLeafs n) | n <- [1 .. k - 1]]
                elseBodyExp = [|error "different size of leafs"|]
                thenBodyExp = [|Leaf ($(zipArgsByBin (varE fName) (namesToExp vNames))) ($(varE $ head sNames))|]
                matchAllNodes =
                    match
                        (tupP nodesPat)
                        (normalB $ bodyToCenter 0)
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
