module Monadoc where

import qualified Control.Monad as Monad
import qualified Data.Version as Version
import qualified Paths_monadoc as This
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as Io
import qualified Text.Printf as Printf

main :: IO ()
main = do
  arguments <- Environment.getArgs
  mainWithArguments arguments

mainWithArguments :: [String] -> IO ()
mainWithArguments arguments = do
  options <- getOptions arguments
  print options

getOptions :: [String] -> IO Options
getOptions arguments = do
  let (updates, unexpecteds, unknowns, errors) = parseArguments arguments
  showUnexpecteds unexpecteds
  showUnknowns unknowns
  Monad.unless (null errors) $ showErrorsAndExit errors
  let options = buildOptions updates
  Monad.when (optionsShowHelp options) showHelpAndExit
  Monad.when (optionsShowVersion options) showVersionAndExit
  pure options

data Options = Options
  { optionsShowHelp :: Bool
  , optionsShowVersion :: Bool
  } deriving (Eq, Show)

descriptions :: [Console.OptDescr (Options -> Options)]
descriptions =
  [ Console.Option
    ['h']
    ["help"]
    (Console.NoArg (\options -> options { optionsShowHelp = True }))
    "show the help"
  , Console.Option
    ['v']
    ["version"]
    (Console.NoArg (\options -> options { optionsShowVersion = True }))
    "show the version"
  ]

parseArguments
  :: [String] -> ([Options -> Options], [String], [String], [String])
parseArguments = Console.getOpt' Console.Permute descriptions

showUnexpecteds :: [String] -> IO ()
showUnexpecteds =
  mapM_ (warnLn . Printf.printf "WARNING: unexpected argument `%s'")

showUnknowns :: [String] -> IO ()
showUnknowns = mapM_ (warnLn . Printf.printf "WARNING: unknown option `%s'")

warnLn :: String -> IO ()
warnLn = Io.hPutStrLn Io.stderr

showErrorsAndExit :: [String] -> IO a
showErrorsAndExit errors = do
  mapM_ (warn . Printf.printf "ERROR: %s") errors
  Exit.exitFailure

warn :: String -> IO ()
warn = Io.hPutStr Io.stderr

buildOptions :: [Options -> Options] -> Options
buildOptions = foldl (\options update -> update options) defaultOptions

defaultOptions :: Options
defaultOptions = Options {optionsShowHelp = False, optionsShowVersion = False}

showHelpAndExit :: IO a
showHelpAndExit = do
  name <- Environment.getProgName
  putStr $ Console.usageInfo (unwords [name, "version", version]) descriptions
  Exit.exitFailure

version :: String
version = Version.showVersion This.version

showVersionAndExit :: IO a
showVersionAndExit = do
  putStrLn version
  Exit.exitFailure
