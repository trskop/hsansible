{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  Tests main
-- Copyright:    (c) 2013, 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- Tests main.
module Main (main)
  where

import System.IO (IO)

import Test.Framework (defaultMain)

import TestCase (tests)


main :: IO ()
main = defaultMain tests
