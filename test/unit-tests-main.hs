-- |
-- Module:       Main
-- Description:  Tests main
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  portable
--
-- Tests main.
module Main (main)
    where

import Test.Framework (defaultMain)

import TestCase (tests)


main :: IO ()
main = defaultMain tests

