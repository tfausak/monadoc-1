module Monadoc where

import Data.Semigroup ((<>))

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Json
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.CaseInsensitive as Case
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Version as Version
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Version as Cabal
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
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
import qualified Text.XML as Xml
import qualified Text.XML.Lens as Lens

defaultMain :: IO ()
defaultMain = do
  arguments <- Environment.getArgs
  mainWithArguments arguments

mainWithArguments :: [String] -> IO ()
mainWithArguments arguments = do
  manager <- Client.newTlsManager
  Client.setGlobalManager manager
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

showErrorsAndExit :: [String] -> IO never
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

showProblemAndExit :: String -> IO never
showProblemAndExit problem = do
  warnLn $ Printf.printf "ERROR: %s" problem
  Exit.exitFailure

showHelpAndExit :: IO never
showHelpAndExit = do
  name <- Environment.getProgName
  putStr $ Console.usageInfo (unwords [name, "version", version]) descriptions
  Exit.exitFailure

version :: String
version = Version.showVersion This.version

showVersionAndExit :: IO never
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

exceptionResponse :: Exception.SomeException -> Server.Response
exceptionResponse _ = jsonResponse Http.status500 [] Json.Null

jsonResponse
  :: Json.ToJSON json
  => Http.Status
  -> Http.ResponseHeaders
  -> json
  -> Server.Response
jsonResponse status headers =
  Server.responseLBS status (addJsonHeader headers) . Json.encode

addJsonHeader :: Http.ResponseHeaders -> Http.ResponseHeaders
addJsonHeader = (jsonHeader :)

jsonHeader :: Http.Header
jsonHeader = (Http.hContentType, jsonMime)

jsonMime :: Bytes.ByteString
jsonMime = toUtf8 "application/json"

toUtf8 :: String -> Bytes.ByteString
toUtf8 = Text.encodeUtf8 . Text.pack

applyMiddleware :: Server.Middleware
applyMiddleware = Server.gzip Server.def . Server.logStdout

application :: Server.Application
application request respond = do
  let handler = getHandler request
  response <- runHandler handler request
  respond response

getHandler :: Server.Request -> Handler
getHandler request = case (requestMethod request, requestPath request) of
  ("GET", [packageName, versionNumber, moduleName]) ->
    getModuleHandler packageName versionNumber moduleName
  _ -> notFoundHandler

type Handler
  = Except.ExceptT String
  ( Reader.ReaderT Server.Request IO
  ) Server.Response

requestMethod :: Server.Request -> String
requestMethod = fromUtf8 . Server.requestMethod

fromUtf8 :: Bytes.ByteString -> String
fromUtf8 = Text.unpack . Text.decodeUtf8

requestPath :: Server.Request -> [String]
requestPath = fmap Text.unpack . Server.pathInfo

getModuleHandler :: String -> String -> String -> Handler
getModuleHandler rawPackage rawVersion rawModule = do
  packageName <- parsePackageName rawPackage
  versionNumber <- parseVersionNumber rawVersion
  moduleName <- parseModuleName rawModule
  let url = makeHaddockUrl packageName versionNumber moduleName
  request <- Client.parseRequest url
  manager <- Trans.lift . Trans.lift $ Client.getGlobalManager
  response <- Trans.lift . Trans.lift $ Client.httpLbs request manager
  input <- case Http.statusCode $ Client.responseStatus response of
    200 -> pure $ Client.responseBody response
    _ -> Except.throwE "failed to get documentation from Hackage"
  original <- either (Except.throwE . Exception.displayException) pure
    $ parseXml input
  let
    document =
      original
        Lens.& styleLens
        Lens..~ styleElement
        Lens.& scriptLens
        Lens..~ scriptElement
    output = renderXml document
  pure $ htmlResponse Http.status200 [] output

parsePackageName
  :: Monad m => String -> Except.ExceptT String m Cabal.PackageName
parsePackageName packageName =
  maybe
      ( Except.throwE . Printf.printf "invalid package name: %s" $ show
        packageName
      )
      pure
    $ Cabal.simpleParse packageName

parseVersionNumber
  :: Monad m => String -> Except.ExceptT String m Cabal.Version
parseVersionNumber versionNumber =
  maybe
      ( Except.throwE . Printf.printf "invalid version number: %s" $ show
        versionNumber
      )
      pure
    $ Cabal.simpleParse versionNumber

parseModuleName
  :: Monad m => String -> Except.ExceptT String m Cabal.ModuleName
parseModuleName moduleName =
  maybe
      ( Except.throwE . Printf.printf "invalid module name: %s" $ show
        moduleName
      )
      pure
    $ Cabal.simpleParse moduleName

makeHaddockUrl
  :: Cabal.PackageName -> Cabal.Version -> Cabal.ModuleName -> String
makeHaddockUrl packageName versionNumber moduleName = concat
  [ "https://hackage.haskell.org/package/"
  , Cabal.unPackageName packageName
  , "-"
  , Cabal.showVersion versionNumber
  , "/docs/"
  , List.intercalate "-" $ Cabal.components moduleName
  , ".html"
  ]

parseXml :: LazyBytes.ByteString -> Either Exception.SomeException Xml.Document
parseXml =
  Xml.parseLBS Xml.def { Xml.psDecodeEntities = Xml.decodeHtmlEntities }

styleLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
styleLens =
  Lens.root Lens../ Lens.named (ci "head") Lens../ Lens.named (ci "link")

ci :: String -> Case.CI Text.Text
ci = Case.mk . Text.pack

styleElement :: Xml.Element
styleElement = Xml.Element
  (String.fromString "link")
  ( Map.map Text.pack . Map.mapKeys String.fromString $ Map.fromList
    [("href", "/todo.css"), ("rel", "stylesheet")]
  )
  []

scriptLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
scriptLens =
  Lens.root Lens../ Lens.named (ci "head") Lens../ Lens.named (ci "script")

scriptElement :: Xml.Element
scriptElement = Xml.Element
  (String.fromString "script")
  ( Map.map Text.pack . Map.mapKeys String.fromString $ Map.fromList
    [("src", "/todo.js")]
  )
  [Xml.NodeContent $ Text.pack ""]

renderXml :: Xml.Document -> LazyBytes.ByteString
renderXml = Xml.renderLBS Xml.def

htmlResponse
  :: Http.Status
  -> Http.ResponseHeaders
  -> LazyBytes.ByteString
  -> Server.Response
htmlResponse status = Server.responseLBS status . addHtmlHeader

addHtmlHeader :: Http.ResponseHeaders -> Http.ResponseHeaders
addHtmlHeader = (htmlHeader :)

htmlHeader :: Http.Header
htmlHeader = (Http.hContentType, htmlMime)

htmlMime :: Bytes.ByteString
htmlMime = toUtf8 "text/html; charset=utf-8"

notFoundHandler :: Handler
notFoundHandler = pure notFoundResponse

notFoundResponse :: Server.Response
notFoundResponse = jsonResponse Http.status404 [] Json.Null

runHandler :: Handler -> Server.Request -> IO Server.Response
runHandler handler request = do
  result <- Reader.runReaderT (Except.runExceptT handler) request
  let response = either responseForProblem id result
  pure response

responseForProblem :: String -> Server.Response
responseForProblem = jsonResponse Http.status500 [] . Json.String . Text.pack
