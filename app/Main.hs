{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import GenTH ()
import Matrix ()

main :: IO ()
main = putStrLn "HI"
