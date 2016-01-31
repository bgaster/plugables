{-|
Module      : Main
Description : Program entry code, argument passing, create and run pipeline
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@email.com
Stability   : experimental
Portability : Not sure!

Entry point for Blocks controller host application. Straightforward, simply
pass any comamnd line arguments, in this case:

   -h help (optional)
   -v version (optional)
   <lookuptable-file> table data for generating MIDI messages, auto generated from
                      instrument controller web application
   -p port            Arduino serial port

Assuming required lookup table file and port as provided, then it simply creates
a Arduino to MIDI pipeline. It asks the user to select a output MIDI device, then
the pipeline works as follows:

    - Arduino sends Module Control Packets
    - Host (i.e. this application) receives, transforms into MIDI messages
    - Resulting MIDI messages are fordwarded to output MIDI device
    - Loop, forever!
-}

module Main (
  main
  ) where

-- standard Haskell libraries

import System.IO
import System.Environment
import System.Exit

import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as S

import Control.Monad.Trans.State
import Pipes

-- now our own modules

import Protocol.Message
import Protocol.Packet
import Midi.Message
import AppState

--------------------------------------------------------------------------------
-- Basic command line processing
--------------------------------------------------------------------------------

data Opts = Opts {
  -- | filename for lookup table data
  tables :: String,
  -- | port for serial communications with the Arduino device
  port   :: String }

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: hostController <lookuptable-file> [-vh] -p <port>"
  exitWith ExitSuccess

showHelp :: IO ()
showHelp = do
  putStrLn "Usage: hostController <lookuptable-file> [-vh] -p <port>\n"
  putStrLn "-h\t this help text"
  putStrLn "-v\t display version string"
  exitWith ExitSuccess

showVersion :: IO ()
showVersion = do
  putStrLn "Block Controllers host application version 0.1"
  exitWith ExitSuccess

-- | Function 'getOpts' reads a list of command line arguments and
--   and parses them resulting, if arguments are correct, in Opts.
getOpts :: IO Opts
getOpts = do xs <- getArgs
             process (Opts "" "") xs
  where
    process opts ("-h":xs)   = showHelp >> undefined
    process opts ("-v":xs)   = process opts xs
    process opts ("-p":p:xs) = process (opts { port = p }) xs
    process opts (x:xs)      = process (opts { tables = x }) xs
    process opts []          = pure opts

--------------------------------------------------------------------------------
-- Arduino to MIDI pipeline
--------------------------------------------------------------------------------

-- | function 'handle' opens comms with Arduino and output MIDI device
--   and returns a producer that generates a stream of Module Control messages
--   from the Arduino
handle :: Opts -> Producer B.ByteString AppM ()
handle opts = do
  lift $ connectController (port opts) S.CS9600 --S.CS57600
  lift $ connectDevice (tables opts)
  (message >> lift disconnectController)

-- | function 'arduinoToMidiPipe' opens comms with Arduino and output MIDI
--   device, then it executes the Arduino -> MIDI pipeline
arduinoToMidiPipe :: Opts -> Effect AppM ()
arduinoToMidiPipe opts = for (handle opts) packet >-> midi

--loop :: Opts -> Effect AppM ()
--loop opts = for (handle opts) (packet ~> lift . liftIO . print)

--------------------------------------------------------------------------------

-- | main function, simply passes command line arguments and starts
--   Arduino to MIDI pipeline
main :: IO ()
main = do
  opts <- getOpts
  if (null (tables opts) || null (port opts)) 
    then showUsage
    else evalStateT (runEffect (arduinoToMidiPipe opts)) emptyAppState
