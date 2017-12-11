module Monadoc where

import Data.Semigroup ((<>))

import qualified Control.Monad as Monad
import qualified Data.Aeson as Json
import qualified Data.ByteString as Bytes
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Server
import qualified Network.Wai.Handler.Warp as Server
import qualified Network.Wai.Middleware.Gzip as Server
import qualified Network.Wai.Middleware.RequestLogger as Server
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
  Server.runSettings settings $ applyMiddleware application

getOptions :: [String] -> IO Options
getOptions arguments = do
  let (updates, unexpecteds, unknowns, errors) = parseArguments arguments
  showUnexpecteds unexpecteds
  showUnknowns unknowns
  Monad.unless (null errors) $ showErrorsAndExit errors
  options <- buildOptionsOrExit updates
  Monad.when (optionsShowHelp options) showHelpAndExit
  Monad.when (optionsShowVersion options) showVersionAndExit
  pure options

data Options = Options
  { optionsHost :: Server.HostPreference
  , optionsPort :: Server.Port
  , optionsShowHelp :: Bool
  , optionsShowVersion :: Bool
  } deriving (Eq, Show)

type Update = Options -> Either String Options

descriptions :: [Console.OptDescr Update]
descriptions =
  [helpDescription, hostDescription, portDescription, versionDescription]

helpDescription :: Console.OptDescr Update
helpDescription = Console.Option
  []
  ["help"]
  (Console.NoArg (\options -> pure options { optionsShowHelp = True }))
  "show the help"

hostDescription :: Console.OptDescr Update
hostDescription = Console.Option
  []
  ["host"]
  ( Console.ReqArg
    (\host options -> pure options { optionsHost = String.fromString host })
    "HOST"
  )
  "host to bind to"

portDescription :: Console.OptDescr Update
portDescription = Console.Option
  []
  ["port"]
  ( Console.ReqArg
    ( \rawPort options -> case Read.readMaybe rawPort of
      Nothing -> fail ("invalid port: " <> show rawPort)
      Just port -> pure options { optionsPort = port }
    )
    "PORT"
  )
  "port to listen on"

versionDescription :: Console.OptDescr Update
versionDescription = Console.Option
  []
  ["version"]
  (Console.NoArg (\options -> pure options { optionsShowVersion = True }))
  "show the version"

parseArguments :: [String] -> ([Update], [String], [String], [String])
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

buildOptionsOrExit :: [Update] -> IO Options
buildOptionsOrExit = either showProblemAndExit pure . buildOptions

buildOptions :: [Update] -> Either String Options
buildOptions = Monad.foldM (\options update -> update options) defaultOptions

defaultOptions :: Options
defaultOptions = Options
  { optionsHost = String.fromString "127.0.0.1"
  , optionsPort = 8080
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
makeSettings options =
  Server.setBeforeMainLoop (beforeMainLoop options)
    . Server.setOnExceptionResponse exceptionResponse
    . Server.setPort (optionsPort options)
    . Server.setServerName mempty
    $ Server.defaultSettings

beforeMainLoop :: Options -> IO ()
beforeMainLoop options = Printf.printf
  "Listening on %s port %d\n"
  (show $ optionsHost options)
  (optionsPort options)

exceptionResponse :: exception -> Server.Response
exceptionResponse _ = jsonResponse Http.status500 [] Json.Null

jsonResponse
  :: Json.ToJSON a
  => Http.Status
  -> Http.ResponseHeaders
  -> a
  -> Server.Response
jsonResponse status headers =
  Server.responseLBS status (addJsonHeader headers) . Json.encode

addJsonHeader :: Http.ResponseHeaders -> Http.ResponseHeaders
addJsonHeader = (jsonHeader :)

jsonHeader :: Http.Header
jsonHeader = (Http.hContentType, jsonMime)

jsonMime :: Bytes.ByteString
jsonMime = Text.encodeUtf8 $ Text.pack "application/json"

applyMiddleware :: Server.Middleware
applyMiddleware = Server.gzip Server.def . Server.logStdout

application :: Server.Application
application _ respond = respond $ jsonResponse Http.status501 [] Json.Null
