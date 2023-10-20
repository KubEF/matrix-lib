{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Matrix ()
import MatrixMapNTH

$(genMaps 10)

main :: IO ()
main = putStrLn "HI"
