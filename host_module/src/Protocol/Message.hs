{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Protocol.Message
Description : Producer interface for reading messages from Arduino controller
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@email.com
Stability   : experimental
Portability : Not sure!

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Protocol.Message (
  SerialPort,
  connectController,
  disconnectController,
  message
  ) where
       
import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as S

import Control.Monad.Trans.State
import Control.Monad (unless)
import Pipes

import Data.ByteString.Builder

--------------------------------------------------------------------------------
-- simple state transformer (with IO) to implictly track the active serialport
-- handle

-- | The 'MessageState' type is the type of values stored in the SerialPort monad
type MessageState = S.SerialPort

-- | The 'SerialPort' type is a state monad (with IO support) used to control 
--   access/communication to the Arduino hardware
type SerialPort = StateT MessageState IO

--------------------------------------------------------------------------------

-- | Message sent by hardware controller, to initiate communication
recvSyncMessage :: B.ByteString
recvSyncMessage = "A"

-- | Message sent by host controller, to sync communication
sendSyncMessage :: B.ByteString
sendSyncMessage = "B"

-- | Message sent by hardware controller, to end communication
-- this is not generally used
terminateMessage :: B.ByteString
terminateMessage = "terminate"

-- | Byte representing the termination of a given message
messageTerminator :: B.ByteString
messageTerminator = "\n"

--------------------------------------------------------------------------------
  
-- FIXME: at least need to catch an error with opening serial port

-- | The 'connectController' function opens a serial connection at a given speed
-- It takes two arguments, of type FilePath and S.CommSpeed, respectively
connectController :: FilePath ->
                     S.CommSpeed ->
                     SerialPort ()
connectController port cspeed = do
  s <- liftIO $ S.openSerial port S.defaultSerialSettings { S.commSpeed = cspeed }
  -- now we need to get in sync with the controller
  --inSync s
  put s
  pure ()
  where inSync s = (liftIO $ S.recv s 1) >>=
                   \v -> do
--                     liftIO $ print v
                     if v == recvSyncMessage
                       then do liftIO $ S.send s sendSyncMessage
                               liftIO $ S.flush s
                               pure ()
                       else inSync s

-- | The 'disconnectController' function closes, an already open, serial
--   connection
disconnectController :: SerialPort ()
disconnectController = get >>= liftIO . S.closeSerial

--------------------------------------------------------------------------------

-- | The 'readMessage' function reads a message from the serial port of the
--   connected Arduino
-- It returns the read message as a 'ByteString'
readMessage :: SerialPort B.ByteString
readMessage = aux "" >>= pure
  where aux bs = do
          v <- get >>= liftIO . flip (S.recv) 1
          if v == ""
            then aux bs
            else do --_ <- lift $ liftIO $ print (toLazyByteString (byteStringHex v))
                    if v == messageTerminator
                      then pure bs
                      else aux $ B.append bs v

---------------------------------------------------------------------------------
-- presume: SerialPort is correctly initialized
-- we are not currently expecting the hardware controller to send
-- terminate message, but future proof it now

-- | The 'message' function produces messages, read from the Arduino's
--   serial port, one at a time via the 'Producer' interface
message :: Producer B.ByteString SerialPort ()
message = do
  msg <- lift readMessage
  unless (msg == terminateMessage) $ do
    yield msg
    message
