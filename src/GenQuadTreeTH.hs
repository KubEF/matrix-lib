{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module GenQuadTreeTH where

import QuadTreeTH (genBinFunc, genBinKFunc)

$(genBinFunc 4)

$(genBinKFunc 3)