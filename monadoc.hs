import qualified Control.Arrow as Arrow
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Json
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.CaseInsensitive as Case
import qualified Data.Fixed as Fixed
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
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

main :: IO ()
main = do
  arguments <- Environment.getArgs
  mainWithArguments arguments

mainWithArguments :: [String] -> IO ()
mainWithArguments arguments = do
  Io.hSetBuffering Io.stdout Io.LineBuffering
  Io.hSetBuffering Io.stderr Io.LineBuffering
  manager <- Client.newTlsManager
  options <- getOptions arguments
  let settings = makeSettings options
  Sql.withConnection (optionsDatabase options) $ \connection -> do
    runMigrations connection
    Server.runSettings settings . applyMiddleware $ makeApplication
      manager
      connection

runMigrations :: Sql.Connection -> IO ()
runMigrations connection = do
  Sql.execute_ connection
    $ toQuery
        "create table if not exists migrations \
        \( id integer not null primary key autoincrement \
        \, dt text not null unique \
        \)"
  mapM_ (runMigration connection) migrations

toQuery :: String -> Sql.Query
toQuery = Sql.Query . Text.pack

runMigration :: Sql.Connection -> (Time.UTCTime, String) -> IO ()
runMigration connection (time, query) = Sql.withTransaction connection $ do
  rows <- Sql.query
    connection
    (toQuery "select count(*) from migrations where dt = ?")
    [time]
  case (rows :: [[Word]]) of
    [[1]] -> pure ()
    _ -> do
      Sql.execute_ connection (toQuery query)
      Sql.execute
        connection
        (toQuery "insert into migrations (dt) values (?)")
        [time]

migrations :: [(Time.UTCTime, String)]
migrations =
  [ ( unsafeUtc 2017 12 30 8 34 0
    , "create table cache \
    \( id integer not null primary key autoincrement \
    \, key text not null unique \
    \, hash text not null \
    \, value blob not null \
    \)"
    )
  ]

unsafeUtc :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Time.UTCTime
unsafeUtc year month day hour minute second =
  Maybe.fromJust $ utc year month day hour minute second

utc :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Maybe Time.UTCTime
utc year month day hour minute second =
  Time.UTCTime <$> Time.fromGregorianValid year month day <*> fmap
    Time.timeOfDayToTime
    (Time.makeTimeOfDayValid hour minute second)

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
  { optionsDatabase :: String
  , optionsHost :: Server.HostPreference
  , optionsPort :: Server.Port
  , optionsShowHelp :: Bool
  , optionsShowVersion :: Bool
  } deriving (Eq, Show)

type Update = Options -> Either String Options

descriptions :: [Console.OptDescr Update]
descriptions =
  [ databaseDescription
  , helpDescription
  , hostDescription
  , portDescription
  , versionDescription
  ]

databaseDescription :: Console.OptDescr Update
databaseDescription = Console.Option
  []
  ["database"]
  ( Console.ReqArg
    (\database options -> pure options { optionsDatabase = database })
    "DATABASE"
  )
  "database to use"

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
      Nothing -> fail $ Printf.printf "invalid port: %s" (show rawPort)
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
showUnexpecteds = mapM_ showUnexpected

showUnexpected :: String -> IO ()
showUnexpected = warnLn . Printf.printf "WARNING: unexpected argument `%s'"

showUnknowns :: [String] -> IO ()
showUnknowns = mapM_ showUnknown

showUnknown :: String -> IO ()
showUnknown = warnLn . Printf.printf "WARNING: unknown option `%s'"

warnLn :: String -> IO ()
warnLn = Io.hPutStrLn Io.stderr

showErrorsAndExit :: [String] -> IO never
showErrorsAndExit errors = do
  mapM_ showError errors
  Exit.exitFailure

showError :: String -> IO ()
showError = warn . Printf.printf "ERROR: %s"

warn :: String -> IO ()
warn = Io.hPutStr Io.stderr

buildOptionsOrExit :: [Update] -> IO Options
buildOptionsOrExit = either showProblemAndExit pure . buildOptions

buildOptions :: [Update] -> Either String Options
buildOptions = Monad.foldM updateOptions defaultOptions

updateOptions :: Options -> Update -> Either String Options
updateOptions options update = update options

defaultOptions :: Options
defaultOptions = Options
  { optionsDatabase = ":memory:"
  , optionsHost = String.fromString "127.0.0.1"
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
    . Server.setServerName Bytes.empty
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
applyMiddleware = compressionMiddleware . loggingMiddleware

compressionMiddleware :: Server.Middleware
compressionMiddleware = Server.gzip Server.def

loggingMiddleware :: Server.Middleware
loggingMiddleware = Server.logStdout

makeApplication :: Client.Manager -> Sql.Connection -> Server.Application
makeApplication manager connection request respond = do
  let
    handler = getHandler request
    env = Env
      { envConnection = connection
      , envManager = manager
      , envRequest = request
      }
  response <- runHandler handler env
  respond response

getHandler :: Server.Request -> Handler
getHandler request = case (requestMethod request, requestPath request) of
  ("GET", []) -> getRootHandler
  ("GET", ["favicon.ico"]) -> getFaviconHandler
  ("GET", ["styles", "haddock"]) -> getHaddockStyleHandler
  ("GET", ["scripts", "haddock"]) -> getHaddockScriptHandler
  ("GET", ["scripts", "math-jax"]) -> getMathJaxScriptHandler
  ("GET", [packageName, versionNumber, moduleName]) ->
    getModuleHandler packageName versionNumber moduleName
  _ -> notFoundHandler

type Handler
  = Except.ExceptT String
  ( Reader.ReaderT Env IO
  ) Server.Response

data Env = Env
  { envConnection :: Sql.Connection
  , envManager :: Client.Manager
  , envRequest :: Server.Request
  }

requestMethod :: Server.Request -> String
requestMethod = fromUtf8 . Server.requestMethod

fromUtf8 :: Bytes.ByteString -> String
fromUtf8 = Text.unpack . Text.decodeUtf8

requestPath :: Server.Request -> [String]
requestPath = fmap Text.unpack . Server.pathInfo

getRootHandler :: Handler
getRootHandler =
  pure
    . htmlResponse Http.status200 []
    $ toLazyUtf8
        "<!doctype html>\
        \<html>\
          \<head>\
            \<title>Monadoc</title>\
          \</head>\
          \<body>\
            \<h1>Monadoc</h1>\
            \<p>\
              \ This site is still a work in progress.\
              \ You can follow the development <a href='https://github.com/tfausak/monadoc'>on GitHub</a>.\
              \ You can also browse some example modules.\
            \</p>\
            \<ul>\
              \<li><a href='/flow/1.0.10/Flow'>Flow from flow 1.0.10</a></li>\
              \<li><a href='/base/4.10.1.0/Prelude'>Prelude from base 4.10.1.0</a></li>\
              \<li><a href='/ghc-prim/0.5.1.1/GHC.Prim'>GHC.Prim from ghc-prim 0.5.1.1</a></li>\
            \</ul>\
          \</body>\
        \</html>"

getFaviconHandler :: Handler
getFaviconHandler =
  pure
    . Server.responseLBS
        Http.status200
        [(Http.hContentType, toUtf8 "image/x-icon")]
    $ LazyBytes.pack
        [ 0x00
        , 0x00
        , 0x01
        , 0x00
        , 0x01
        , 0x00
        , 0x01
        , 0x01
        , 0x00
        , 0x00
        , 0x01
        , 0x00
        , 0x18
        , 0x00
        , 0x2c
        , 0x00
        , 0x00
        , 0x00
        , 0x16
        , 0x00
        , 0x00
        , 0x00
        , 0x28
        , 0x00
        , 0x00
        , 0x00
        , 0x01
        , 0x00
        , 0x00
        , 0x00
        , 0x02
        , 0x00
        , 0x00
        , 0x00
        , 0x01
        , 0x00
        , 0x18
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x08
        , 0x00
        , 0x00
        , 0x00
        , 0x28
        , 0x00
        , 0x00
        , 0x00
        , 0x28
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x86
        , 0x50
        , 0x5e
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        , 0x00
        ]

getHaddockStyleHandler :: Handler
getHaddockStyleHandler = pure $ cssResponse Http.status200 [] haddockStyle

cssResponse
  :: Http.Status
  -> Http.ResponseHeaders
  -> LazyBytes.ByteString
  -> Server.Response
cssResponse status = Server.responseLBS status . addCssHeader

addCssHeader :: Http.ResponseHeaders -> Http.ResponseHeaders
addCssHeader = (cssHeader :)

cssHeader :: Http.Header
cssHeader = (Http.hContentType, cssMime)

cssMime :: Bytes.ByteString
cssMime = toUtf8 "text/css"

haddockStyle :: LazyBytes.ByteString
haddockStyle = toLazyUtf8 "/* TODO */"

toLazyUtf8 :: String -> LazyBytes.ByteString
toLazyUtf8 = LazyText.encodeUtf8 . LazyText.pack

getHaddockScriptHandler :: Handler
getHaddockScriptHandler = pure $ jsResponse Http.status200 [] haddockScript

jsResponse
  :: Http.Status
  -> Http.ResponseHeaders
  -> LazyBytes.ByteString
  -> Server.Response
jsResponse status = Server.responseLBS status . addJsHeader

addJsHeader :: Http.ResponseHeaders -> Http.ResponseHeaders
addJsHeader = (jsHeader :)

jsHeader :: Http.Header
jsHeader = (Http.hContentType, jsMime)

jsMime :: Bytes.ByteString
jsMime = toUtf8 "application/javascript"

haddockScript :: LazyBytes.ByteString
haddockScript = toLazyUtf8 "/* TODO */"

getMathJaxScriptHandler :: Handler
getMathJaxScriptHandler = pure $ Server.responseLBS
  Http.status302
  [ ( Http.hLocation
    , toUtf8
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    )
  ]
  LazyBytes.empty

getModuleHandler :: String -> String -> String -> Handler
getModuleHandler rawPackage rawVersion rawModule = do
  packageName <- parsePackageName rawPackage
  versionNumber <- parseVersionNumber rawVersion
  moduleName <- parseModuleName rawModule
  let url = makeHaddockUrl packageName versionNumber moduleName
  request <- Client.parseRequest url
  manager <- Trans.lift $ Reader.asks envManager
  response <- Trans.lift . Trans.lift $ Client.httpLbs request manager
  input <- case Http.statusCode $ Client.responseStatus response of
    200 -> pure $ Client.responseBody response
    _ -> Except.throwE "failed to get documentation from Hackage"
  original <- either (Except.throwE . Exception.displayException) pure
    $ parseXml input
  let
    document =
      original
        Lens.& haddockStyleLens
        Lens..~ haddockStyleElement
        Lens.& haddockScriptLens
        Lens..~ haddockScriptElement
        Lens.& mathJaxScriptLens
        Lens..~ mathJaxScriptElement
        Lens.& javaScriptLens
        Lens..~ javaScriptElement
    output = renderXml document
  pure $ htmlResponse Http.status200 [] output

parsePackageName
  :: Monad m => String -> Except.ExceptT String m Cabal.PackageName
parsePackageName packageName =
  maybeToExcept (Printf.printf "invalid package name: %s" (show packageName))
    $ Cabal.simpleParse packageName

maybeToExcept :: Monad m => e -> Maybe a -> Except.ExceptT e m a
maybeToExcept problem = maybe (Except.throwE problem) pure

parseVersionNumber
  :: Monad m => String -> Except.ExceptT String m Cabal.Version
parseVersionNumber versionNumber
  = maybeToExcept
      (Printf.printf "invalid version number: %s" (show versionNumber))
    $ Cabal.simpleParse versionNumber

parseModuleName
  :: Monad m => String -> Except.ExceptT String m Cabal.ModuleName
parseModuleName moduleName =
  maybeToExcept (Printf.printf "invalid module name: %s" (show moduleName))
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

haddockStyleLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
haddockStyleLens =
  Lens.root
    Lens../ Lens.named (toCi "head")
    Lens../ Lens.named (toCi "link")
    . Lens.attributeIs (String.fromString "href") (Text.pack "ocean.css")

toCi :: String -> Case.CI Text.Text
toCi = Case.mk . Text.pack

haddockStyleElement :: Xml.Element
haddockStyleElement =
  xmlElement "link" [("href", "/styles/haddock"), ("rel", "stylesheet")] []

xmlElement :: String -> [(String, String)] -> [Xml.Node] -> Xml.Element
xmlElement name = Xml.Element (String.fromString name) . Map.fromList . fmap
  (String.fromString Arrow.*** Text.pack)

haddockScriptLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
haddockScriptLens =
  Lens.root
    Lens../ Lens.named (toCi "head")
    Lens../ Lens.named (toCi "script")
    . Lens.attributeIs (String.fromString "src") (Text.pack "haddock-util.js")

haddockScriptElement :: Xml.Element
haddockScriptElement =
  xmlElement "script" [("src", "/scripts/haddock")] [emptyXmlNode]

emptyXmlNode :: Xml.Node
emptyXmlNode = Xml.NodeContent Text.empty

mathJaxScriptLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
mathJaxScriptLens =
  Lens.root
    Lens../ Lens.named (toCi "head")
    Lens../ Lens.named (toCi "script")
    . Lens.attributeIs
        (String.fromString "src")
        ( Text.pack
          "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        )

mathJaxScriptElement :: Xml.Element
mathJaxScriptElement =
  xmlElement "script" [("src", "/scripts/math-jax")] [emptyXmlNode]

javaScriptLens :: Lens.Simple Lens.Traversal Xml.Document Xml.Element
javaScriptLens =
  Lens.root
    Lens../ Lens.named (toCi "head")
    Lens../ Lens.named (toCi "script")
    . Lens.attributeIs (String.fromString "type") (Text.pack "text/javascript")

javaScriptElement :: Xml.Element
javaScriptElement = xmlElement "script" [] [emptyXmlNode]

renderXml :: Xml.Document -> LazyBytes.ByteString
renderXml = Xml.renderLBS Xml.def
  { Xml.rsAttrOrder = const Map.toAscList
  , Xml.rsXMLDeclaration = False
  }

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

runHandler :: Handler -> Env -> IO Server.Response
runHandler handler env = do
  result <- Reader.runReaderT (Except.runExceptT handler) env
  let response = either responseForProblem id result
  pure response

responseForProblem :: String -> Server.Response
responseForProblem = jsonResponse Http.status500 [] . Json.String . Text.pack
