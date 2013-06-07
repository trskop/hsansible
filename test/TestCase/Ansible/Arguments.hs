{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for raw (no parsing) Ansible module arguments
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable
-- Portability:  non-portable (OverloadedStrings, depends on non-portable
--               module)
--
-- Tests for raw (no parsing) Ansible module arguments.
module TestCase.Ansible.Arguments (tests)
    where

import Control.Applicative ((<$>))
import Data.String (fromString)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Ansible.Arguments


tests, rawArgumentsTests, standardArgumentsTests, castBoolTests,
    fromPairsTests :: [Test]

tests =
    [ testGroup "Raw module arguments" rawArgumentsTests
    , testGroup "Standard module arguments" standardArgumentsTests
    , testGroup "Function castBool" castBoolTests
    , testGroup "Function fromPairs" fromPairsTests
    ]

rawArgumentsTests =
    [ testCase "empty arguments"
        $ Right (RawArguments "") @=? parseArguments ""
    , testCase "white space"
        $ Right (RawArguments "  \n  \r \t \r\n")
            @=? parseArguments "  \n  \r \t \r\n"
    , testCase "UTF8 string"
        $ Right (RawArguments "φιλοσοφία") @=? parseArguments "\207\134\206\
        \\185\206\187\206\191\207\131\206\191\207\134\206\175\206\177"
    ]

standardArgumentsTests =
    [ testCase "empty arguments"
        $ Just [] @=? parseStdArguments ""

    , testCase "empty arguments, but with white space on input"
        $ Just [] @=? parseStdArguments "  \t   \t\t"

    , testCase "key with no value"
        $ Just [("foo", Nothing)] @=? parseStdArguments "foo"

    , testCase "key with no value with white space on the left"
        $ Just [("foo", Nothing)] @=? parseStdArguments "  foo"

    , testCase "key with no value with white space on the right"
        $ Just [("foo", Nothing)] @=? parseStdArguments "foo  "

    , testCase "key with no value surrounded by white space"
        $ Just [("foo", Nothing)] @=? parseStdArguments "  foo  "

    , testCase "key with empty value"
        $ Just [("foo", Just "")] @=? parseStdArguments "foo="

    , testCase "key with single quoted value"
        $ Just [("foo", Just "")] @=? parseStdArguments "foo=''"

    , testCase "key with double quoted value"
        $ Just [("foo", Just "")] @=? parseStdArguments "foo=\"\""

    , testCase "key with ASCII value"
        $ Just [("foo", Just "bar")] @=? parseStdArguments "foo=bar"

    , testCase "key with single quoted ASCII value"
        $ Just [("foo", Just "bar")] @=? parseStdArguments "foo='bar'"

    , testCase "key with double quoted ASCII value"
        $ Just [("foo", Just "bar")] @=? parseStdArguments "foo=\"bar\""

    , testCase "key with UTF8 value"
        $ Just [("foo", Just "φιλοσοφία")] @=? parseStdArguments "foo=\207\
        \\134\206\185\206\187\206\191\207\131\206\191\207\134\206\175\206\177"

    , testCase "key with single quoted UTF8 value"
        $ Just [("foo", Just "φιλοσοφία")] @=? parseStdArguments "foo='\207\
        \\134\206\185\206\187\206\191\207\131\206\191\207\134\206\175\206\177'"

    , testCase "key with double quoted UTF8 value"
        $ Just [("foo", Just "φιλοσοφία")] @=? parseStdArguments "foo=\"\207\
        \\134\206\185\206\187\206\191\207\131\206\191\207\134\206\175\206\177\""

    , testCase "two keys without values"
        $ Just [("foo", Nothing), ("bar", Nothing)]
            @=? parseStdArguments "foo bar"

    , testCase "two keys without values with a lot of whitespace in between"
        $ Just [("foo", Nothing), ("bar", Nothing)]
            @=? parseStdArguments "foo  \t  bar"

    , testCase "three keys with empty values and various kind of quoting"
        $ Just [("foo", Just ""), ("bar", Just ""), ("baz", Just "")]
            @=? parseStdArguments "foo='' bar= baz=\"\""

    , testCase "three keys with empty values, various kind of quoting with\
      \ more white space in between"
        $ Just [("foo", Just ""), ("bar", Just ""), ("baz", Just "")]
            @=? parseStdArguments "foo='' \t  bar=\t  baz=\"\""

    , testCase "invalid key name produces parse error"
        $ Nothing @=? parseStdArguments "az_@$$"
    ]

castBoolTests =
    [ testCase "yes -> Just True"
        $ replicate 4 (Just True) @=? map castBool ["yes", "Yes", "YES", "yEs"]

    , testCase "no -> Just False"
        $ replicate 4 (Just False) @=? map castBool ["no", "No", "NO", "nO"]

    , testCase "true -> Just True"
        $ replicate 3 (Just True) @=? map castBool ["true", "True", "tRUe"]

    , testCase "false -> Just False"
        $ replicate 4 (Just False) @=? map castBool ["false", "False", "FALSE", "faLsE"]

    , testCase "1/0 -> Just True/False"
        $ [Just True, Just False] @=? map castBool ["1", "0"]

    , testCase "foo, bar, baz -> Nothing"
        $ replicate 3 Nothing @=? map castBool ["foo", "bar", "baz"]
    ]

fromPairsTests =
    [ testCase "empty input" $ Just (Nothing :: Maybe String)
        @=? fromPairs (\ _ _ -> return (Just "")) []
    , testProperty "identyty" fromPairsIdentityProperty
    ]
  where
    fromPairsIdentityProperty :: [(String, Maybe String)] -> Bool
    fromPairsIdentityProperty kvps =
        let kvps' = map (\ (k, v) -> (fromString k, fromString <$> v)) kvps
        in fromPairs (((:) .) . (,)) kvps' [] == kvps'
