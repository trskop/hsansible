{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Framework for building up Ansible modules.
-- Copyright:    (c) 2013, 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, OverloadedStrings
--
-- Very simple framework that allows building Ansible modules by just following
-- the type signature of 'moduleMain' type signature.
module Ansible
    (
    -- * Ansible module
      Module
    , moduleMain

    -- * Errors/failures

    -- | Ansible requres failures to be a JSON messages with certain form. Use
    -- interface provided by 'ExceptT' monad transformer and its 'ExceptT class
    -- for creating and throwing errors.
    , Failure()

    -- * Module arguments

    -- | Most Ansible modules conform to simple @key=value@ format of its
    -- arguments and it's the format we use here. User may use 'StdArguments',
    -- that provide just parsed key=value pairs, or build his/her own type.
    , ParseArguments(..)
    , RawArguments(..)
    , StdArguments(..)

    -- * Utility functions
    , castBool
    , fromPairs
    , thenFail
    , otherwiseFail
    )
  where

-- {{{ Imports ----------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad (Monad((>>), fail, return), when)
import Data.Bool (Bool, not)
import Data.Either (Either(Left, Right))
import Data.Function ((.), ($))
import Data.List (drop, head, null, take)
import Data.Maybe (Maybe(Nothing), isNothing)
import Data.String (String)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Data.Aeson as JSON
import Data.ByteString.Lazy.Char8 as BS (putStrLn, readFile)

import Ansible.Arguments
import Ansible.Failure

-- }}} Imports ----------------------------------------------------------------


-- | Ansible module is parametrized by arguments (@a@) and it either produces
-- 'Failure' or a result @r@.
type Module a b m r = a -> Maybe b -> ExceptT Failure m r

-- | Execute Module in most cases it's going to look like:
--
-- > main :: IO ()
-- > main = Ansible.moduleMain ansibleModuleImplementation
moduleMain
    :: (FromJSON b, MonadIO m, ParseArguments a, ToJSON r)
    => Module a b m r
    -> m ()
moduleMain ansibleModule = do
    result <- Except.runExceptT $ do
        args <- liftIO $ take 2 <$> getArgs
        when (null args) $ fail "Arguments file wasn't passed."
        moduleArgs <- readArgumentsFile $ head args
        complexArgs <- case drop 1 args of
            [f] -> do
                x <- liftIO $ JSON.decode <$> BS.readFile f
                isNothing x `thenFail` "Parsing of complex arguments failed."
                return x
            _ -> return Nothing -- No complex arguments were passed
        ansibleModule moduleArgs complexArgs
    case result of
        Right x -> printJson x
        Left x -> printJson x >> liftIO exitFailure
  where
    printJson :: (MonadIO m, ToJSON a) => a -> m ()
    printJson = liftIO . BS.putStrLn . JSON.encode . JSON.toJSON

-- {{{ Utility functions ------------------------------------------------------

-- | Call @'Monad'.'fail'@, with specified message, when predicate is 'True'.
thenFail :: Monad m => Bool -> String -> m ()
thenFail p = when p . fail
infix 0 `thenFail`

-- | Call @'Monad'.'fail'@, with specified message, when predicate is 'False'.
otherwiseFail :: Monad m => Bool -> String -> m ()
otherwiseFail = thenFail . not
infix 0 `otherwiseFail`

-- }}} Utility functions ------------------------------------------------------
