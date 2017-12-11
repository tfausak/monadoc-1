module Monadoc where

import qualified Control.Monad as Monad
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Server
import qualified Network.Wai.Handler.Warp as Server
import qualified Paths_monadoc as This
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as Io
import qualified Text.Printf as Printf
import qualified Text.Read as Read

main :: IO ()
main = do
  arguments <- Environment.getArgs
  mainWithArguments arguments

mainWithArguments :: [String] -> IO ()
mainWithArguments arguments = do
  options <- getOptions arguments
  let settings = makeSettings options
  Server.runSettings settings application

getOptions :: [String] -> IO Options
getOptions arguments = do
  let (updates, unexpecteds, unknowns, errors) = parseArguments arguments
  showUnexpecteds unexpecteds
  showUnknowns unknowns
  Monad.unless (null errors) $ showErrorsAndExit errors
  options <- either showProblemAndExit pure $ buildOptions updates
  Monad.when (optionsShowHelp options) showHelpAndExit
  Monad.when (optionsShowVersion options) showVersionAndExit
  pure options

data Options = Options
  { optionsPort :: Server.Port
  , optionsShowHelp :: Bool
  , optionsShowVersion :: Bool
  } deriving (Eq, Show)

descriptions :: [Console.OptDescr (Options -> Either String Options)]
descriptions =
  [ Console.Option
    ['h']
    ["help"]
    (Console.NoArg (\options -> pure options { optionsShowHelp = True }))
    "show the help"
  , Console.Option
    ['p']
    ["port"]
    ( Console.ReqArg
      ( \rawPort options -> do
        port <- Read.readEither rawPort
        pure options { optionsPort = port }
      )
      "PORT"
    )
    "port to listen on"
  , Console.Option
    ['v']
    ["version"]
    (Console.NoArg (\options -> pure options { optionsShowVersion = True }))
    "show the version"
  ]

parseArguments
  :: [String]
  -> ([Options -> Either String Options], [String], [String], [String])
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

buildOptions :: [Options -> Either String Options] -> Either String Options
buildOptions = foldl (flip $ either Left) $ pure defaultOptions

defaultOptions :: Options
defaultOptions = Options
  { optionsPort = 8080
  , optionsShowHelp = False
  , optionsShowVersion = False
  }

showProblemAndExit :: String -> IO a
showProblemAndExit problem = do
  warnLn $ Printf.printf "ERROR: %s" problem
  Exit.exitFailure

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

makeSettings :: Options -> Server.Settings
makeSettings options = Server.setServerName mempty
  $ Server.setPort (optionsPort options) Server.defaultSettings

application :: Server.Application
application _ respond = respond $ Server.responseLBS Http.status501 [] mempty
