{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  Simple "echo" module for Ansible
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (OverloadedStrings)
--
-- Simple "echo" Ansible module that either prints its arguments (reformated in
-- to JSON) or fails when called with "fail" argument.
module Main (main)
    where

import Control.Monad (when)
import Data.Maybe (isJust)
import System.Environment (getArgs)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON (Value, object)

import qualified Ansible


main :: IO ()
main = Ansible.moduleMain $ \ stdArgs@(Ansible.StdArguments args) cmplx -> do
    cmdArgs <- liftIO getArgs
    when (isJust $ lookup "fail" args) $ fail "Requested failure is here."
    return $ JSON.object
        [ "parsedModuleArguments" .= stdArgs
        , "complexModuleArguments" .= (cmplx :: Maybe JSON.Value)
        , "commandLineArguments" .= cmdArgs
        ]
