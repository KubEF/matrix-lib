{-# LANGUAGE TemplateHaskell #-}

module QuadTreeTH where

import Language.Haskell.TH
import QuadTree

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
-- leafWithArgs l1 v1 s1 = l1@(Leaf v1 s1)
-- note that v1 and s1 are not names, so you have to wrap wishing names into pattern variables
leafWithArgs :: (Quote m) => Name -> m Pat -> m Pat -> m Pat
leafWithArgs name v s = asP name [p|Leaf $v $s|]

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