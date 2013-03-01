module URLShortener (getTinyURL
    , getISGDURL
    , getVGDURL
) where

import Network.HTTP


tinyURL :: String
tinyURL = "http://tinyurl.com/api-create.php?url="

isgdURL :: String
isgdURL = "http://is.gd/create.php?format=simple&logstats=1&url="

vgdURL :: String
vgdURL = "http://v.gd/create.php?format=simple&logstats=1&url="

getURL :: String -> String -> IO String
getURL b u = do
        answer <- simpleHTTP $ getRequest $ b ++ u
        case answer of
            Left _ -> return "Got a connection error"
            Right result -> return $ rspBody result

getTinyURL ::  String -> IO String
getTinyURL = getURL tinyURL

getISGDURL ::  String -> IO String
getISGDURL = getURL isgdURL

getVGDURL ::  String -> IO String
getVGDURL = getURL vgdURL

