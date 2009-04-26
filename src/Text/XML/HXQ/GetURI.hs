module Text.XML.HXQ.GetURI where

import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.IO (hPutStrLn, stderr)

err :: String -> IO (String)
err msg = do 
          hPutStrLn stderr msg
          return ""

get :: URI -> IO (String)
get uri =
    do
    resp <- simpleHTTP (request uri)
    either (err . show) parseResponse resp

parseResponse :: Response String ->IO (String)
parseResponse resp = 
    case rspCode resp of
                      (2,0,0) -> return $ (rspBody resp)
                      (3,0,1) -> maybe (return "") getURI (lookupHeader HdrLocation (rspHeaders resp))
                      (3,0,2) -> maybe (return "") getURI (lookupHeader HdrLocation (rspHeaders resp))
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp


request :: URI -> Request [Char]
request uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

getURI :: String -> IO (String)
getURI uri = maybe (readFile uri) get (parseURI uri)


