{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import GenQuadTreeTH
import GenTH
import Helpers
import Matrix
import ParseMTX
import QuadTree

main :: IO ()
main = putStrLn "HI"
