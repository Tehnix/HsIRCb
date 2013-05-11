{-# LANGUAGE OverloadedStrings #-}
module Src.Modules.BTCInfo (getBTCInfo
                            , getBTCProfit
                            , getBTCPrice
) where
    
import Network.HTTP
import Data.Aeson ((.:), (.:?), eitherDecode, FromJSON)
import Data.Aeson.Types
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as BS


data BTCInfo = BTCInfo { getHigh       :: String
                       , getLow        :: String
                       , getAvg        :: String
                       , getLast       :: String
                       } deriving (Eq, Show)

instance FromJSON BTCInfo where
    parseJSON (Object v) = BTCInfo <$>
        (((v .: "return") >>= (.: "high")) >>= (.: "value")) <*>
        (((v .: "return") >>= (.: "low")) >>= (.: "value")) <*>
        (((v .: "return") >>= (.: "avg")) >>= (.: "value")) <*>
        (((v .: "return") >>= (.: "last")) >>= (.: "value"))
    parseJSON _ = mzero

getJSON :: String -> IO String
getJSON u = do
    answer <- simpleHTTP $ getRequest u
    case answer of
        Left _ -> return "Got a connection error"
        Right result -> return $ rspBody result

decodeBTCInfo :: BS.ByteString -> Either String (Maybe BTCInfo)
decodeBTCInfo = eitherDecode

getBTCInfo :: String -> IO String
getBTCInfo currency = do
    rawJSON <- getJSON $ "http://data.mtgox.com/api/1/BTC" ++ currency ++ "/ticker"
    json <- return $ BS.pack rawJSON
    btc <- return $ decodeBTCInfo json
    case btc of
        Left err -> return err
        Right btcjson -> case btcjson of
            Just btcinfo -> return $ getLast btcinfo
            Nothing -> return "Couldn't find it :("

getBTCProfit :: IO String
getBTCProfit = do
    curPrice <- getBTCInfo "EUR"
    let priceInFloat = read curPrice
    let profit = floor $ (priceInFloat - 56) * 5.30397956 * 7.45
    return $ show profit ++ " DKK"

getBTCPrice :: IO String
getBTCPrice = do
    curPriceInEuro <- getBTCInfo "EUR"
    curPriceInUSD <- getBTCInfo "USD"
    curPriceInDKK <- getBTCInfo "DKK"
    let priceInEuro = show $ floor $ read curPriceInEuro
    let priceInUSD = show $ floor $ read curPriceInUSD
    let priceInDKK = show $ floor $ read curPriceInDKK
    return $ "1btc: " ++ priceInEuro ++ "euro, $" ++ priceInUSD ++ ", " ++ priceInDKK ++ "DKK"
