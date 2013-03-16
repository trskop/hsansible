-- |
-- Module:       Main
-- Description:  Tests main
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable
-- Portability:  non-portable (depends on non-portable module)
--
-- Tests main.
module Main (main)
    where

import Test.Framework (Test, defaultMain, testGroup)

import qualified Test.Ansible.Arguments as Arguments (tests)


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Module Ansible.Arguments" Arguments.tests
    ]
