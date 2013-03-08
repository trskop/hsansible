{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Main
-- Description:  Create Ansible module out of binary executable.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   dogmat@gmail.com | peter.trsko@gmail.com
-- Stability:    unstable | experimental | provisional | stable | frozen
-- Portability:  non-portable (OverloadedStrings RecordWildCards)
--
-- While Ansible allows it's users to develop modules in any scripting language
-- it's sometimes useful to be able to use compiled languages. This program
-- allows us to do so, by wrapping binary executable in to python wrapper. Such
-- wrapper contains Base 64 encoded binary as part of its body. Wrapped
-- executable has to follow Ansible module interface, but other than that there
-- aren't any additional limitations.
module Main (main)
    where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (unless, when)
import Data.String (fromString)
import Data.Monoid (Endo(..), First(..), Monoid(..))
import Data.Version (Version, showVersion)
import System.Console.GetOpt
    (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)

import qualified Data.ByteString as BS (readFile, writeFile, putStr)
import qualified Data.ByteString.Base64 as Base64 (encode, joinWith)
import qualified Text.StringTemplate as ST

import Paths_hsansible (getDataFileName, version)


-- | Data type holds default configuration values and values obtained via
-- command line options. Any changes in the action, i.e. what program does are
-- separate from it.
data Config = Config
    { progName :: String
    , progVersion :: Version
    , defaultTemplateFile :: FilePath
    , templateFile :: FilePath
    , binaryFile :: FilePath
    , documentationFile :: Maybe FilePath
    , outputFile :: Maybe FilePath
    -- ^ Nothing is interpreted as 'stdout'.
    }

-- | Print help message that includes usage information, to specified file
-- handle. It starts with empty line.
printHelp :: Handle -> Config -> IO ()
printHelp h Config{..} = do
    hPutStrLn h . flip usageInfo options $ unlines
        [ ""
        , "Create Ansible module out of binary executable that follows the\
            \ Ansible module interface."
        , ""
        , "Usage:"
        , "    " ++ progName ++ " [-o OUTPUT_FILE] [-t TEMPLATE_FILE]\
            \ [-d DOC_FILE] BINARY_FILE"
        , "    " ++ progName ++ " {-h|-V|--numeric-version|--print-template}"
        ]
    hPutStrLn h $ unlines
        [ "Path to Ansible module template:"
        , ""
        , "    " ++ defaultTemplateFile
        ]

-- | Description of program options.
options :: [OptDescr (Endo Config, First (Config -> IO ()))]
options =
    [ Option "h" ["help"] (NoArg $ action printHelp')
        "Show this help and exit."

    , Option "V" ["version"]
        (NoArg . action $ printVersion False)
        "Show version string and exit."

    , Option "" ["numeric-version"]
        (NoArg . action $ printVersion True)
        "Show machine readable version number and exit."

    , Option "t" ["template"] (ReqArg (set tmplFile) "TEMPLATE_FILE")
       "Set custom template instead of the default."

    , Option "o" ["output"] (ReqArg (set outFile) "OUTPUT_FILE")
        "Set output to OUTPUT_FILE instead of stdout."

    , Option "d" ["doc", "documentation"] (ReqArg (set docFile) "DOC_FILE")
        "Read module documentation from DOC_FILE, it has to be a Ansible-style\
        \ module documentation in YAML."

    , Option "" ["print-template"] (NoArg $ action printTemplate)
        "Print default template and exit. This is great as a starting point\
        \ for your own template file."
    ]
  where
    set f str = (Endo $ f str, First Nothing)
    action f = (Endo id, First $ Just f)

    tmplFile str cfg = cfg{templateFile = str}
    outFile str cfg = cfg{outputFile = Just str}
    docFile str cfg = cfg{documentationFile = Just str}

    printHelp' cfg = printVersion False cfg >> printHelp stdout cfg
    printTemplate Config{..} = readFile defaultTemplateFile >>= putStr
    printVersion p Config{..} = putStrLn $
        (if p then id else (progName ++) . (' ' :)) (showVersion progVersion)

-- | Generate configuration using default values and process environment.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
    prog <- getProgName
    templateFile <- getDataFileName "data/ansible-module.st"
    return Config
        { progName = prog
        , progVersion = version
        , defaultTemplateFile = templateFile
        , templateFile = templateFile
        , binaryFile = ""
        , documentationFile = Nothing
        , outputFile = Nothing
        }

-- | Main functionality of this program, it takes 'Config' and produces Ansible
-- module either to @stdout@ or to specific file, depending on the value of
-- 'outputFile'.
generateAnsibleModule :: Config -> IO ()
generateAnsibleModule Config{..} = do
    template <- ST.newSTMP <$> readFile templateFile
    documentation <- case documentationFile of
        Just fp -> Just <$> BS.readFile fp
        Nothing -> return Nothing
    encodedBinary <- (Base64.joinWith "\n" 79 . Base64.encode)
        <$> BS.readFile binaryFile
    maybe BS.putStr BS.writeFile outputFile . ST.render
        . flip ST.setManyNativeAttrib template $
            maybe id ((:) . (,) "documentation") documentation
                [ ("program", fromString progName)
                , ("version", fromString $ showVersion progVersion)
                , ("encodedBinary", encodedBinary)
                ]

-- | Main function just parses options, gets default values and delegates real
-- work to other functions.
main :: IO ()
main = do
    defaultConfig <- mkDefaultConfig

    (m, rest, errs) <- getOpt Permute options <$> getArgs
    unless (null errs)
        $ die defaultConfig errs

    case first (($ defaultConfig) . appEndo) $ mconcat m of
        (cfg, First (Just action)) -> action cfg
        (cfg, First Nothing) -> do
            when (length (take 2 rest) /= 1) $ die cfg ["Missing input file.\n"]
            generateAnsibleModule $ setBinaryFile (head rest) cfg
  where
    setBinaryFile fp cfg = cfg{binaryFile = fp}

    die :: Config -> [String] -> IO a
    die cfg msgs = do
        hPutStr stderr . concat $ map ((progName cfg ++ ": ERROR: ") ++) msgs
        printHelp stderr cfg
        exitFailure
