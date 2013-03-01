module HsIRCb where

import Data.List
import Data.Char (isDigit)
import Network
import System.IO
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import qualified Control.Exception as E
import Text.Printf

import HsIRCParser.HsIRCParser
import URLShortener (getTinyURL, getISGDURL, getVGDURL)
import Gamble (realDice, rollDice, coinToss)

server ::  String
server = "irc.codetalk.io"
port ::  Integer
port   = 6667
chan ::  String
chan   = "#lobby"
user ::  String
user   = "HsIRCb"
nick ::  String
nick   = "LambdaBot-junior"
 
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify = E.bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    register
    asks socket >>= listen

-- Register your connection on the IRC network, and join a channel
register :: Net ()
register = do
    write "NICK" nick
    write "USER" (user ++ " 0 * :HsIRCb")
    write "JOIN" chan

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    t <- return $ tokenize s
    io (putStrLn s)
    if ping s then pong s 
        else if isCode t then evalCode (getCode t) 
            else eval (clean s)
  where
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Dispatch a command
eval :: String -> Net ()
eval ".list"         = privmsg "help, uptime, realdice, dice, coin, id, tiny, short, safeshort"
-- eval ".quit"         = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval ".uptime"       = uptime >>= privmsg
eval ".whoisawesome" = privmsg "Em| is the awesomest"
eval ".realdice"     = iprivmsg realDice
eval ".dice"         = io rollDice >>= iprivmsg
eval ".coin"         = io coinToss >>= iprivmsg
eval x | ".id " `isPrefixOf` x       = privmsg $ drop 4 x
eval x | ".tiny" `isPrefixOf` x      = io (getTinyURL (drop 6 x)) >>= privmsg
eval x | ".short" `isPrefixOf` x     = io (getISGDURL (drop 7 x)) >>= privmsg
eval x | ".safeshort" `isPrefixOf` x = io (getVGDURL (drop 11 x)) >>= privmsg
eval _                               = return () -- ignore everything else

-- Look for IRC codes
evalCode :: String -> Net ()
evalCode x | "451" `isInfixOf` x = register
evalCode _                       = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a privmsg to the current chan + server, and auto convert from int
-- to string
iprivmsg :: Int -> Net ()
iprivmsg i = write "PRIVMSG" $ chan ++ " :" ++ (show i)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- Pretty print the date in '1d 9h 9m 17s' format
pretty :: TimeDiff -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td,[]) metrics

