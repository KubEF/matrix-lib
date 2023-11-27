{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module GenQuadTreeTH where

import QuadTreeTH (genBinFunc, genBinKFunc, genMulWithElement)

$(genBinFunc 4)

$(genBinKFunc 3)
$(genMulWithElement)