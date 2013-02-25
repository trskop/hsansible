{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       $HEADER$
-- Description:  Framework for building up Ansible modules.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (OverloadedStrings)
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
    -- interface provided by 'ErrorT' monad transformer and its 'Error' class
    -- for creating and throwing errors.
    , Failure()

    -- * Module arguments

    -- | Most Ansible modules conform to simple key=value format of its
    -- arguments and it's the format we use here. User may use 'StdArguments',
    -- that provide just parsed key=value pairs, or build his/her own type.
    , ParseArguments(..)
    , RawArguments(..)
    , StdArguments(..)
    )
    where

-- {{{ Imports ----------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Error (ErrorT)
import qualified Control.Monad.Trans.Error as Error
import Data.Aeson as JSON
import Data.ByteString.Lazy.Char8 as BS (putStrLn, readFile)

import Ansible.Arguments
import Ansible.Failure

-- }}} Imports ----------------------------------------------------------------


-- | Ansible module is parametrized by arguments (@a@) and it either produces
-- 'Failure' or a result @r@.
type Module a b m r = a -> Maybe b -> ErrorT Failure m r

-- | Execute Module in most cases it's going to look like:
--
-- > main :: IO ()
-- > main = Ansible.moduleMain ansibleModuleImplementation
moduleMain
    :: (FromJSON b, MonadIO m, ParseArguments a, ToJSON r)
    => Module a b m r
    -> m ()
moduleMain ansibleModule = do
    result <- Error.runErrorT $ do
        args <-  liftIO $ take 2 <$> getArgs
        when (null args) $ fail "Arguments file wasn't passed."
        moduleArgs <- readArgumentsFile $ head args
        complexArgs <- case drop 1 args of
            [f] -> do
                x <- liftIO $ JSON.decode <$> BS.readFile f
                case x of
                    Nothing -> fail "Parsing of complex arguments failed."
                    _ -> return x
            _ -> return Nothing -- No complex arguments were passed
        ansibleModule moduleArgs complexArgs
    case result of
        Right x -> printJson x
        Left x -> printJson x >> liftIO exitFailure
  where
    printJson :: (MonadIO m, ToJSON a) => a -> m ()
    printJson = liftIO . BS.putStrLn . JSON.encode . JSON.toJSON
