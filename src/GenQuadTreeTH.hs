{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GenQuadTreeTH where

import QuadTreeTH (genBinFunc)

$(genBinFunc 4)
