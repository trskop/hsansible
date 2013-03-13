{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Main
-- Description:  Simple tool for testing Hsansible based Ansible modules
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (RecordWildCards)
--
-- Simple tool for testing Hsansible based Ansible modules. Should also work
-- for any Ansible module that doesn't use Ansible module common code and
-- accept arguments via temporary file.
--
-- It takes module file and copies it in to a temporary file. Creates temporary
-- files for simple and complex module arguments passed via @-a@ or @-A@, and
-- @-c@ or @-C@ options. After doing so it executes copy of the module and
-- passes argument files to it as command line options.
module Main (main)
    where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (unless, when)
import Data.Monoid (Endo(..), First(..), Monoid(..))
import Data.Version (Version, showVersion)
import System.Console.GetOpt
    (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitWith)
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)

import System.Cmd (rawSystem)
import System.Directory
    ( copyFile
    , emptyPermissions
    , setOwnerExecutable
    , setOwnerReadable
    , setPermissions
    )
import System.FilePath ((</>), takeFileName)
import System.IO.Temp (withSystemTempDirectory)

import Paths_hsansible (version)


-- | Data type holds default configuration values and values obtained via
-- command line options. Any changes in the action, i.e. what program does are
-- separate from it.
data Config = Config
    { progName :: String
    , progVersion :: Version
    , moduleFile :: FilePath
    , moduleArguments :: Maybe String
    , moduleArgumentsFile :: Maybe FilePath
    , moduleComplexArgumentsFile :: Maybe FilePath
    , moduleComplexArguments :: Maybe String
    }
    deriving (Show)

-- | Print help message that includes usage information, to specified file
-- handle. It starts with empty line.
printHelp :: Handle -> Config -> IO ()
printHelp h Config{..} = hPutStrLn h . flip usageInfo options $ unlines
    [ ""
    , "Execute ansible module with specified arguments passed to it via\
      \ temporary files."
    , ""
    , "Usage:"
    , "    " ++ progName ++ " [-a MODULE_ARGUMENTS|-A FILE]\
        \  [-c COMPLEX_ARGUMENTS|-C FILE] MODULE_FILE"
    , "    " ++ progName ++ " {-h|-V|--numeric-version|--print-template}"
    ]

-- | Description of program options.
options :: [OptDescr (Endo Config, First (Config -> IO ()))]
options =
    [ Option "h" ["help"]
        (NoArg $ action printHelp')
        "Show this help and exit."

    , Option "V" ["version"]
        (NoArg . action $ printVersion False)
        "Show version string and exit."

    , Option "" ["numeric-version"]
        (NoArg . action $ printVersion True)
        "Show machine readable version number and exit."

    , Option "a" ["arguments", "args"]
        (ReqArg (set args) "MODULE_ARGUMENTS")
        "Pass MODULE_ARGUMENTS to the module via temporary file. Don't mix\
        \ with -A option."

    , Option "A" ["arguments-file", "args-file"]
        (ReqArg (set argsFile) "FILE")
        "Copy FILE to a temporary file that will be passed to the module as\
        \ arguments file. Don't mix with -a option."

    , Option "c" ["complex-arguments"]
        (ReqArg (set cmplxArgs) "COMPLEX_ARGUMENTS")
        "Pass COMPLEX_ARGUMENTS to the module via temporary file. Don't mix\
        \ with -C option."

    , Option "C" ["complex-arguments-file"]
        (ReqArg (set cmplxArgsFile) "FILE")
        "Copy FILE to a temporary file that will be passed to the module as\
        \ complex arguments file. Don't mix with -c option."
    ]
  where
    set f str = (Endo $ f str, First Nothing)
    action f = (Endo id, First $ Just f)

    args str cfg = cfg{moduleArguments = Just str}
    argsFile str cfg = cfg{moduleArgumentsFile = Just str}
    cmplxArgs str cfg = cfg{moduleComplexArguments = Just str}
    cmplxArgsFile str cfg = cfg{moduleComplexArgumentsFile = Just str}

    printHelp' cfg = printVersion False cfg >> printHelp stdout cfg
    printVersion p Config{..} = putStrLn $
        (if p then id else (progName ++) . (' ' :)) (showVersion progVersion)

-- | Generate configuration using default values and process environment.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
    prog <- getProgName
    return Config
        { progName = prog
        , progVersion = version
        , moduleFile = ""
        , moduleArguments = Nothing
        , moduleArgumentsFile = Nothing
        , moduleComplexArgumentsFile = Nothing
        , moduleComplexArguments = Nothing
        }

main' :: Config -> FilePath -> IO ()
main' Config{..} tmpDir = do
    mapM_ handleArgsFile
        [ (argsFile,
            moduleArguments, moduleArgumentsFile, "")
        , (cmplxArgsFile,
            moduleComplexArguments, moduleComplexArgumentsFile, "{}")
        ]
    copyFile moduleFile isolatedModuleFile
    setPermissions isolatedModuleFile . setOwnerExecutable True
        $ setOwnerReadable True emptyPermissions
    rawSystem isolatedModuleFile [argsFile, cmplxArgsFile] >>= exitWith
  where
    argsFile, cmplxArgsFile, isolatedModuleFile :: FilePath
    argsFile = tmpDir </> "arguments"
    cmplxArgsFile = tmpDir </> "complex-arguments"
    isolatedModuleFile = tmpDir </> takeFileName moduleFile

    handleArgsFile
        :: (FilePath, Maybe String, Maybe FilePath, String)
        -> IO ()
    handleArgsFile (outFn, content, filepath, defaultContent) = do
        case (content, filepath) of
            (Nothing, Nothing) -> writeFile outFn defaultContent
            (Just args, Nothing) -> writeFile outFn args
            (Nothing, Just fn) -> copyFile fn outFn
            _ -> error "This shouldn't happen: Expecting either file content\
                \ or file name, not both."
                -- This case should be handled by main function.
        setPermissions outFn $ setOwnerReadable True emptyPermissions

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
        (cfg@Config{..}, First Nothing) -> do
            when (length (take 2 rest) /= 1)
                $ die cfg ["Missing module file.\n"]
            dieIfBothSet cfg moduleArguments moduleArgumentsFile
                "Either specify -a or -A, but not both of them.\n"
            dieIfBothSet cfg moduleComplexArguments moduleComplexArgumentsFile
                "Either specify -c or -C, but not both of them.\n"
            withSystemTempDirectory "ansible-module"
                $ main' (setModuleFile cfg $ head rest)
  where
    setModuleFile cfg fp = cfg{moduleFile = fp}

    die :: Config -> [String] -> IO a
    die cfg msgs = do
        hPutStr stderr . concat $ map ((progName cfg ++ ": ERROR: ") ++) msgs
        printHelp stderr cfg
        exitFailure

    dieIfBothSet :: Config -> Maybe a -> Maybe b -> String -> IO ()
    dieIfBothSet cfg (Just _) (Just _) msg = die cfg [msg]
    dieIfBothSet _   _        _        _   = return ()
