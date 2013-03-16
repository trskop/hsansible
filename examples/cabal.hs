#!/usr/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module:       Main
-- Description:  Simple Ansible module for Cabal package installation
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (OverloadedStrings, RecordWildCards,
--               ScopedTypeVariables)
--
-- Simple Ansible module for Cabal package installation.
--
-- Dependencies: aeson, base, case-insensitive, hsansible, process, text and
-- transformers.
module Main (main)
    where

import Data.Monoid (Endo(..), Monoid(..))
import Data.Maybe (fromJust, isJust, isNothing)
import System.Exit (ExitCode(..))

import Ansible
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON (Value, object)
import qualified Data.CaseInsensitive as CI (mk, original)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import System.Process (readProcessWithExitCode)


data Conf = Conf
    { packageName :: Maybe Text
    , updatePackageCache :: Bool
    }

instance ParseArguments Conf where
    parseArguments bs = case parseArguments bs of
        Left msg -> Left msg
        Right (StdArguments args) -> case fromPairs (pairToConf . CI.mk) args of
            Left msg -> Left msg
            Right (Endo f) -> Right . f $ Conf Nothing False
      where
        pairToConf key val
          | key `elem` ["package", "pkg", "name"] =
            Right $ maybe mempty setPackageName val
          | key == "update_cache" || key == "update-cache" =
            -- Presence of "update_cache" key without value is interpreted as
            -- True.
            case maybe (Just True) castBool val of
                Nothing -> Left $ concat
                    [ "Unable to parse argument: "
                    , Text.unpack $ CI.original key
                    , "='", Text.unpack (fromJust val), "'."
                    ]
                Just p -> Right $ setUpdatePackageCache p
          | otherwise = Right mempty
            -- Other options are ignored. It includes "state" since
            -- cabal-install doesn't support uninstall.

        setPackageName x = Endo $ \ c -> c {packageName = Just x}
        setUpdatePackageCache x = Endo $ \ c -> c {updatePackageCache = x}

main :: IO ()
main = Ansible.moduleMain $ \ Conf{..} (_ :: Maybe JSON.Value) -> do
    updateResult <- if updatePackageCache
        then cabal ["update"]
        else return Nothing

    installResult <- if isJust packageName
        then cabal ["install", Text.unpack $ fromJust packageName]
        else return Nothing

    isNothing updateResult && isNothing installResult
        `thenFail` "Either specify package to install or that cache has to be updated."

    return $ JSON.object
        [ "changed" .= isJust installResult
        , "update_result" .= updateResult
        , "install_result" .= installResult
        ]
  where
    mkMsg rc out err = showString "rc: " . shows rc
        . (if null out then id else showString " stdout: " . shows out)
        $ (if null err then id else showString " stderr: " . shows err) ""

    cabal args = do
        (rc, out, err) <- liftIO $ readProcessWithExitCode "cabal" args ""
        rc /= ExitSuccess `thenFail` mkMsg rc out err
        return $ Just (out, err)
