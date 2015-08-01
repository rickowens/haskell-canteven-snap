{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
  A more or less random collection of Snap shorthand and utilities that
  make webservice programming easier.
-}

module Canteven.Snap (
  requiredParam,
  requiredUtf8Param,
  badRequest,
  created,
  noContent,
  notFound,
  conflict,
  methodNotAllowed,
  assertMethod,
  writeJSON,
  unsupportedMediaType,
  readJSON,
  getMethod,
  exactPath,
  showMethod,
  static,
  staticMarkdown,
  staticSnap,
  serverError,
  ContentType,
  RequestEntity(..),
  ResponseEntity(..),
  readEntity,
  writeEntity,
  DecodeResult(..),
  setServerVersion
) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (ToJSON, encode, eitherDecode, FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Default (def)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Version (Version, showVersion)
import Language.Haskell.TH (Exp(LitE, VarE, AppE), Lit(StringL), Q, runIO)
import Snap.Core (Snap, getParam, writeBS, setResponseCode, getResponse,
  finishWith, modifyResponse, setHeader, readRequestBody, Method(Method),
  getsRequest, rqMethod, rqPathInfo, pass, getHeader)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Markdown (markdown)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)
import qualified Data.Text as T (unpack, pack)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import qualified System.Log.Logger as L (debugM, warningM)

{- |
  Look up a parameter, and if it doesn't exist, then cause a 400
  BadRequest to be returned to the user
-}
requiredParam :: ByteString -> Snap ByteString
requiredParam name = do
  param <- getParam name
  case param of
    Nothing -> badRequest ("missing parameter: " ++ d name)
    Just value -> return value


{- |
  Look up a parameter, and decode it using utf8. If the parameter doesn't
  exist, then cause a 400 BadReqeust to be returned to the user.
-}
requiredUtf8Param :: ByteString -> Snap Text
requiredUtf8Param = fmap decodeUtf8 . requiredParam


{- |
  Using this will short-circuit the snap monad (in much the same way as
  `fail` does for all monads), but instead the regular meaning of "fail"
  in the snap monad (which allows the framework to choose some other
  path), this short-circuit causes a 400 Bad Request to be returned no
  matter what.
-}
badRequest :: String -> Snap a
badRequest reason = do -- snap monad
  writeBS (encodeUtf8 (T.pack (reason ++ "\n")))
  response <- fmap (setResponseCode 400) getResponse
  finishWith response


{- |
  Return a @201 Created@ response.
-}
created :: Snap ()
created = modifyResponse (setResponseCode 201)


{- |
  Return a @204 No Content@ response.
-}
noContent :: Snap ()
noContent = modifyResponse (setResponseCode 204)


{- |
  Short circuit with a @404 Not Found@.
-}
notFound :: Snap a 
notFound = do
  response <- fmap (setResponseCode 404) getResponse
  finishWith response


{- |
  Set the reponse code to @409 Conflict@.
-}
conflict :: Snap ()
conflict = modifyResponse (setResponseCode 409)



{- |
  Short circuit with a 405 Method Not Allowed. Also, set the Allow header
  with the allowed methods.
-}
methodNotAllowed :: [Method] -> Snap a
methodNotAllowed allowedMethods = do
  response <- fmap (withAllow . setResponseCode 405) getResponse
  finishWith response
  where
    withAllow = (
        setHeader "Allow"
        . encodeUtf8
        . T.pack
        . intercalate ", "
        . map showMethod
      ) allowedMethods


{- |
  Asserts that the request was made using a particular HTTP request method. If
  the assertion fails, then the request will result in a 405 Method Not
  Allowed.
-}
assertMethod :: Method -> Snap a -> Snap a
assertMethod allowedMethod snap = do
  requestMethod <- getsRequest rqMethod
  if requestMethod == allowedMethod
    then snap
    else methodNotAllowed [allowedMethod]


{- |
  Write a `ToJSON` instance to the entity body, setting the @Content-Type@
  header to @application/json@.
-}
writeJSON :: (ToJSON json) => json -> Snap ()
writeJSON j = do
  modifyResponse (setHeader "Content-Type" "application/json")
  writeBS . toStrict . encode $ j


{- |
  Short circuit with a 415 response.
-}
unsupportedMediaType :: Snap a
unsupportedMediaType = finishWith . setResponseCode 415 =<< getResponse


{- |
  Read a `FromJSON` from the request entity, causing a 400 bad request on
  a parsing error. The first argument specifies the maximum size we are
  willing to read. This method delegates to `Snap.Core.readRequestBody`,
  so an exception will be thrown in the case where the maximum size
  is exceeded.
-}
readJSON :: (FromJSON json) => Int64 -> Snap json
readJSON maxSize = do
  body <- readRequestBody maxSize
  case eitherDecode body of
    Left err -> do
      logReason body err 
      badRequest err
    Right json -> return json
  where
    logReason body err = warningM (
        "readJSON failed because of " ++ show err ++ " on " ++ show body
      )


{- |
  Shorthand Snap action for retrieving the request method.
-}
getMethod :: Snap Method
getMethod = getsRequest rqMethod


{- |
  Make sure that the path info is empty. If it isn't, then fail the snap action
  using `pass`. This is helpful when using routes to make sure that a route
  named "/foo" does not match an request uri of "/foo/bar"
-}
exactPath :: Snap a -> Snap a
exactPath s = do
  pathInfo <- getsRequest rqPathInfo
  liftIO $ debugM ("PathInfo: " ++ show pathInfo)
  case pathInfo of
    "" -> s
    _ -> pass


{- |
  The default `show` implementation of `Method` isn't good enough,
  specifically the value `show (Method "FOO")` == `"Method \"Foo\""`, which is
  unsuitable for use when displaying the actual name of the method in a formal
  context (read: in the `Allow` header).
-}
showMethod :: Method -> String
showMethod (Method m) = (T.unpack . decodeUtf8) m
showMethod m = show m


{- |
  This is a template haskell function you can use to automatically include
  static files in the executable. The resulting `Q Exp` will have the type
  `Snap ()`.
-}
static
  :: FilePath
    -- ^ The compile-time location of file containing the static content.
  -> String
    -- ^ The content type that should be reported.
  -> Q Exp
static filename contentType = runIO $ do
  fileStr <- readFile filename
  return $
    VarE 'staticSnap
      `AppE` LitE (StringL fileStr)
      `AppE` LitE (StringL contentType)


{- |
  This is a template haskell function you can use to automatically include
  markdown files in the executable. The resulting `Q Exp` will have the type
  `Snap ()`.
-}
staticMarkdown
  :: FilePath
    -- ^ The compile-time location of file containing the static content.
  -> Q Exp
staticMarkdown filename = runIO $ do
    fileContent <- TL.decodeUtf8 <$> L.readFile filename
    return $
      VarE 'staticSnap
        `AppE` LitE (StringL (renderMarkdown fileContent))
        `AppE` LitE (StringL "text/html")
  where
    renderMarkdown :: TL.Text -> String
    renderMarkdown = renderHtml . markdown def 


{- |
  Create a Snap that returns static content.
-}
staticSnap :: ByteString -> ContentType -> Snap ()
staticSnap bs ct = do
  modifyResponse (setHeader "Content-Type" ct)
  exactPath (writeBS bs)
 

{- |
  Return a server error, no matter what.
-}
serverError :: String -> Snap a
serverError reason = do -- snap monad
  writeBS (encodeUtf8 (T.pack (reason ++ "\n")))
  response <- fmap (setResponseCode 500) getResponse
  finishWith response


{- |
  decode a bytestring to a string, via utf8
-}
d :: ByteString -> String
d = T.unpack . decodeUtf8


{- |
  Shorthand logging
-}
debugM :: String -> IO ()
debugM = L.debugM "canteven-snap"


{- |
  Shorthand logging
-}
warningM :: (MonadIO io) => String -> io ()
warningM = liftIO . L.warningM "canteven-snap"


{- |
  ContentType is an alias for ByteString
-}
type ContentType = ByteString


{- |
  The class of things that can be read as request entitys.

  Strictly speaking, this is not bound to snap, but it is used
  to facilitate `readEntity` and `writeEntity`, which *are* snap
  functions. At some point, it may be valuable to pull this out into a
  separate library.
-}
class RequestEntity e where
  {- |
    Decode the entity, according to the specified content type.
  -}
  decodeEntity :: Maybe ContentType -> L.ByteString -> DecodeResult e


{- |
  The class of things that can be used as http response entities.

  Strictly speaking, this is not bound to snap, but it is used
  to facilitate `readEntity` and `writeEntity`, which *are* snap
  functions. At some point, it may be valuable to pull this out into a
  separate library.
-}
class ResponseEntity e where
  {- |
    The content type of the respone entity.
  -}
  getContentType :: e -> ContentType

  {- |
    The bytes associated with the response entity.
  -}
  getBytes :: e -> ByteString


{- |
  Reads and decodes a request entity, returning the appropriate error
  responses if the entity can't be decoded: @415 Unsupported Media Type@ if the
  content type is not supported, or @400 Bad Request@ if the content type is
  supported, but the entity can't be decoded.
-}
readEntity :: (RequestEntity e) => Int64 -> Snap e
readEntity maxSize = do
  body <- readRequestBody maxSize
  contentType <- getRequestContentType
  case decodeEntity contentType body of
    Unsupported -> do
      logUnsupported contentType
      unsupportedMediaType
    BadEntity reason -> do
      logBadEntity contentType reason
      badRequest reason
    Ok e -> return e
  where
    getRequestContentType = getsRequest (getHeader "content-type")
    logBadEntity contentType reason = warningM (
        "readEntity of " ++ show contentType ++
        " failed because of: " ++ show reason
      )
    logUnsupported contentType = warningM (
        "readEntity failed because " ++ show contentType ++
        " is not supported."
      )


{- |
  A more powerful version of writeJSON. This function writes generic
  `ResponseEntity`s, and sets the right content type for them.
-}
writeEntity :: (ResponseEntity e) => e -> Snap()
writeEntity e = do
  modifyResponse (setHeader "Content-Type" (getContentType e))
  writeBS . getBytes $ e


{- |
  The result of trying to decode a request entity.
-}
data DecodeResult e
  = Unsupported
    -- ^ Signifies an unsupported content type.
  | BadEntity String
    -- ^ Signifies that the request entity is invalid, and provides some
    --   kind of reason why.
  | Ok e
    -- ^ Successfully decoded the entity.


{- |
  Utility method that sets the @Server:@ header to something sensible.
-}
setServerVersion
  :: String
    -- ^ @name@ - The name of your server.
  -> Version
    -- ^ @version@ - The version of your server.
  -> Snap ()
setServerVersion name =
  modifyResponse
  . setHeader "Server"
  . encodeUtf8
  . T.pack
  . ((name ++ "/") ++)
  . showVersion


