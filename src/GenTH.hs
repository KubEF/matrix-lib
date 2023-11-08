{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GenTH where

import ListTH
import MatrixMapNTH

$(genMaps 10)

$(genThroughFuncMatrix 4)

$(genListToTupleFrom2ToK 62)