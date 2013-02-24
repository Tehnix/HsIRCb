module HsIRCb where

import Data.List
import Data.Char (isDigit)
import Network
import System.IO
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import Control.OldException
import Text.Printf
import Prelude hiding (catch)

import TinyURL (getTinyURL)
import Gamble (realDice, rollDice, coinToss)
 
server = "irc.codetalk.io"
port   = 6667
chan   = "#lobby"
user   = "HsIRCb"
nick   = "hsbot-YesTehnixIsToyingAround"
 
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (const $ return ())

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

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
    io (putStrLn s)
    if ping s then pong s 
    else if isCode s then evalCode (getCode s) 
         else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Dispatch a command
eval :: String -> Net ()
eval "!quit"       = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval "!uptime"     = uptime >>= privmsg
eval "!realdice"   = privmsg (show realDice)
eval "!dice"       = do 
    r <- io $ rollDice
    privmsg (show r)
eval "!coin"       = do 
    r <- io $ coinToss
    privmsg (show r)
eval x | "!id " `isPrefixOf` x   = privmsg $ drop 4 x
eval x | "!tiny " `isPrefixOf` x = do
    url <- io $ getTinyURL (drop 6 x)
    privmsg url
eval _                           = return () -- ignore everything else

-- Look for IRC codes
evalCode :: String -> Net ()
evalCode x | "451" `isInfixOf` x = register
evalCode _                       = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

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


{-| TO BE SUBSTITUTED BY THE HsIRCParser module -}
-- Split a string on a given delimiter
split :: String -> Char -> [String]
split str delim = let (start, end) = break (== delim) str
    in start : if null end then [] else split (tail end) delim

-- Tokenize a string
tokenize ::  String -> [String]
tokenize s = tail $ split s ':'

-- Check if there is an IRC code present
isCode :: String -> Bool
isCode s = let w = words (head (tokenize s)) in if length w > 1 then all isDigit (w!!1) else False

-- Grab the IRC code from the output
getCode :: String -> String
getCode s = let w = words (head (tokenize s)) in w!!1
