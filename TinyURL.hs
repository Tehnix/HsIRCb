module TinyURL (getTinyURL) where

import Network.HTTP


tinyURL = "http://tinyurl.com/api-create.php?url="

getTinyURL :: String -> IO String
getTinyURL u = do
    answer <- simpleHTTP $ getRequest (tinyURL ++ u)
    case answer of
      Left connErr -> return $ "Got a connection error..."
      Right result -> return $ rspBody result
