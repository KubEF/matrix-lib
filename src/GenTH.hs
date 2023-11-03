{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GenTH where

import MatrixMapNTH

$(genMaps 10)

$(genThroughFuncMatrix 4)