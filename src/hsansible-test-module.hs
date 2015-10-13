{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  Simple "echo" module for Ansible
-- Copyright:    (c) 2013, 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, OverloadedStrings
--
-- Simple "echo" Ansible module that either prints its arguments (reformated in
-- to JSON) or fails when called with "fail" argument.
module Main (main)
  where

import Control.Monad (Monad(fail, return), when)
import Data.Function (($))
import Data.List (lookup)
import Data.Maybe (Maybe, isJust)
import System.Environment (getArgs)
import System.IO (IO)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON (Value, object)

import qualified Ansible


main :: IO ()
main = Ansible.moduleMain $ \stdArgs@(Ansible.StdArguments args) cmplx -> do
    cmdArgs <- liftIO getArgs
    when (isJust $ lookup "fail" args) $ fail "Requested failure is here."
    return $ JSON.object
        [ "parsedModuleArguments" .= stdArgs
        , "complexModuleArguments" .= (cmplx :: Maybe JSON.Value)
        , "commandLineArguments" .= cmdArgs
        ]
