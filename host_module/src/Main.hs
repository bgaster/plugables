{-|
Module      : Main
Description : Program entry code, argument passing, create and run pipeline
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@email.com
Stability   : experimental
Portability : Not sure!
-}

import System.IO
import System.Environment
import System.Exit

import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as S

import Control.Monad.Trans.State
import Pipes

import Protocol.Message
import Protocol.Packet
import Midi.Message
import AppState

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Opts = Opts { tables :: String,
                   port   :: String }

showUsage = do
  putStrLn "Usage: hostController <lookuptable-file> [-vh] -p <port>"
  exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts "" "") xs
  where
    process opts ("-h":xs)   = process opts xs
    process opts ("-v":xs)   = process opts xs
    process opts ("-p":p:xs) = process (opts { port = p }) xs
    process opts (x:xs)      = process (opts { tables = x }) xs
    process opts []          = opts


handle :: Opts -> Producer B.ByteString AppM ()
handle opts = do
--  let port = "/dev/cu.wchusbserial1410"
  lift $ connectController (port opts) S.CS9600 --S.CS57600
  lift $ connectDevice (tables opts)
  (message >> lift disconnectController)

arduinoToMidiPipe :: Opts -> Effect AppM ()
arduinoToMidiPipe opts = for (handle opts) packet >-> midi

loop :: Opts -> Effect AppM ()
loop opts = for (handle opts) (packet ~> lift . liftIO . print)

main :: IO ()
main = do
  opts <- getOpts
  if (null (tables opts) || null (port opts)) 
    then showUsage
    else evalStateT (runEffect (loop opts)) emptyAppState
