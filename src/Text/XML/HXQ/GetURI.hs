module Text.XML.HXQ.GetURI where

import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.IO (hPutStrLn, stderr)

err :: String -> IO (Maybe String)
err msg = do 
          hPutStrLn stderr msg
          return Nothing

get :: URI -> IO (Maybe String)
get uri =
    do
    resp <- simpleHTTP (request uri)
    either (err . show) parseResponse resp

parseResponse :: Response String ->IO (Maybe String)
parseResponse resp = 
    case rspCode resp of
                      (2,0,0) -> return $ Just (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp


request :: URI -> Request [Char]
request uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

getURI :: String -> IO (Maybe String)
getURI uri = maybe (readFile uri >>= return . Just) get (parseURI uri)


