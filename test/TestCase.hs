{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2013, 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  NoImplicitPrelude; depends on non-portable module.
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.Ansible.Arguments as Arguments (tests)


tests :: [Test]
tests =
    [ testGroup "Module Ansible.Arguments" Arguments.tests
    ]
