{-# LANGUAGE TemplateHaskell #-}

module ListTH where

import Language.Haskell.TH

genZipWithBin :: Int -> Q [Dec]
genZipWithBin k = sequenceA [sigD name typ, funD name [cl1, cl2]]
    where
        name = mkName $ "zipWithBin" ++ show k
        xListStr = ['x' : show n | n <- [1 .. k]]
        xList = map mkName xListStr
        xsList = map (\x -> mkName $ x ++ "s") xListStr
        cl1 = do
            function <- newName "f"
            let pat = (varP function) : [[p|($(varP x) : $(varP xs))|] | (x, xs) <- (zip xList xsList)]
            let calculate =
                    foldl
                        ( \acc x -> [|$(infixE (Just acc) (varE function) (Just $ varE x))|]
                        )
                        (varE $ head xList)
                        (tail xList)
                        :: Q Exp
            let remains = foldl (\acc xs -> [|$acc $(varE xs)|]) (appE (varE name) (varE function)) xsList :: Q Exp
            let body = [|$calculate : $remains|]
            clause pat (normalB body) []
        cl2 = do
            let wilds = replicate (k + 1) wildP :: [Q Pat]
            clause wilds (normalB [|[]|]) []
        typ = do
            let t1 = varT $ mkName "t1"
            let t2 = varT $ mkName "t2"
            let t2List = replicate (k - 1) [t|[$t2]|]
            let funT = makeFunc [t1, t2, t1]
            makeFunc $ funT : ([t|[$t1]|] : t2List) ++ [[t|[$t1]|]]

generateZipWithBinTok :: Int -> Q [Dec]
generateZipWithBinTok k = do
    ls <- mapM genZipWithBin [2 .. k]
    return $ concat ls

genZipWithN :: Int -> Q [Dec]
genZipWithN k = sequenceA [sigD name typ, funD name [cl1, cl2]]
    where
        name = mkName $ "myZipWith" ++ show k
        xListStr = ['x' : show n | n <- [1 .. k]]
        xList = map mkName xListStr
        xsList = map (\x -> mkName $ x ++ "s") xListStr
        cl1 = do
            function <- newName "f"
            let pat = (varP function) : [[p|($(varP x) : $(varP xs))|] | (x, xs) <- (zip xList xsList)]
                calculate =
                    foldl
                        (\acc x -> [|$acc $(varE x)|])
                        (varE function)
                        xList
                        :: Q Exp
                remains = foldl (\acc xs -> [|$acc $(varE xs)|]) (appE (varE name) (varE function)) xsList :: Q Exp
                body = [|$calculate : $remains|]
            clause pat (normalB body) []
        cl2 = do
            let wilds = replicate (k + 1) wildP :: [Q Pat]
            clause wilds (normalB [|[]|]) []
        typ = do
            let resT = varT $ mkName "r"
                fun = makeFunc $ [varT x | x <- xList] ++ [resT]
                lists = [[t|[$(varT x)]|] | x <- xList]
            makeFunc $ fun : lists ++ [[t|[$resT]|]]

generateZipWithTok :: Int -> Q [Dec]
generateZipWithTok k = do
    ls <- mapM genZipWithN [2 .. k]
    return $ concat ls

genListToTuple :: Int -> Q [Dec]
genListToTuple k = sequenceA [sigD name typ, funD name [cl1]]
    where
        name = mkName $ "listToTuple" ++ show k
        listName = mkName "list"
        listExp = varE listName :: Q Exp
        listPat = varP listName :: Q Pat
        xNames = [mkName $ "x" ++ show x | x <- [1 .. k]]
        xPat = map varP xNames :: [Q Pat]
        xExp = map varE xNames :: [Q Exp]
        cl1 = do
            let patList = listPat
                matches =
                    match (listP (take k xPat)) (normalB (tupE (take k xExp))) []
                        : [match wildP (normalB [|error "you can not do tuple with this input"|]) []]
                        :: [Q Match]
                body = caseE listExp matches
            clause [patList] (normalB body) []
        typ = do
            let tuple = foldl appT (tupleT k) (replicate k (varT $ mkName "a"))
            makeFunc [[t|[$(varT $ mkName "a")]|], tuple]

genListToTupleFrom2ToK :: Int -> Q [Dec]
genListToTupleFrom2ToK k = do
    ls <- mapM genListToTuple [2 .. k]
    return $ concat ls

makeFunc :: (Quote m) => [m Type] -> m Type
makeFunc = foldr1 (appT . appT arrowT)