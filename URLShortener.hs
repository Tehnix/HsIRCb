module URLShortener (getTinyURL
    , getISGDURL
    , getVGDURL
) where

import Network.HTTP


tinyURL = "http://tinyurl.com/api-create.php?url="
isgdURL = "http://is.gd/create.php?format=simple&logstats=1&url="
vgdURL = "http://v.gd/create.php?format=simple&logstats=1&url="

getTinyURL :: String -> IO String
getTinyURL u = do
    answer <- simpleHTTP $ getRequest (tinyURL ++ u)
    case answer of
      Left connErr -> return $ "Got a connection error..."
      Right result -> return $ rspBody result

getISGDURL :: String -> IO String
getISGDURL u = do
    answer <- simpleHTTP $ getRequest (isgdURL ++ u)
    case answer of
      Left connErr -> return $ "Got a connection error..."
      Right result -> return $ rspBody result

getVGDURL :: String -> IO String
getVGDURL u = do
    answer <- simpleHTTP $ getRequest (vgdURL ++ u)
    case answer of
      Left connErr -> return $ "Got a connection error..."
      Right result -> return $ rspBody result
