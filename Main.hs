module Main where
    
import HsIRCb
import Control.Monad.Reader
import Network
import System.IO
import qualified Control.Exception as E


-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = E.bracket connect disconnect loop
  where
    disconnect = hClose . socket
    {-loop st    = E.catch (runReaderT run st) (const $ return ())-}
    loop st    = E.catch (runReaderT run st) (\(E.SomeException _) -> return ())