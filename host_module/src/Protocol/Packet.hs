{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Protocol.Packet
Description : Producer interface for symbolic representation of messages
              from Arduino controller
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@gmail.com
Stability   : experimental
Portability : Not sure!


-}
module Protocol.Packet (
  ControlID,
  ControlValue,
  ModuleID,
  ControlPacket(..),
  ModuleControllerPacket(..),
  packet
  ) where

-- standard Haskell libraries

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Pipes

import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Word

-- now our own modules

import Protocol.Message
import AppState

-----------------------------------------------------------------------------
-- Datatypes for control and module packets
-----------------------------------------------------------------------------

-- | Type 'ControlPacket' represents a control packet in the Arduino to host
--   protocol
data ControlPacket =
  ControlPacket
  {
    -- | Controller ID
    controlId    :: ControlID,
    -- | MIDI control value for control command
    controlValue :: ControlValue
  }
  deriving (Show, Eq)

-- | Type 'ModuleControlPacket' represents a module control packet in the
--   Arduino to host protocol
data ModuleControllerPacket =
  ModuleControllerPacket
  {
    -- | Module ID
    moduleID       :: ModuleID,
    -- | Sequence of controller packets received from Arduino
    controlPackets :: [ControlPacket]
  }
  deriving (Show, Eq)  

-----------------------------------------------------------------------------
-- getters from binary form of messages into control and module
-- representations
-----------------------------------------------------------------------------

-- | The 'getControlPacket' tries to read two 'Word8's from a 'ByteString',
--   representing controller ID and control value
getControlPacket :: Get ControlPacket
getControlPacket = do
  ctrId  <- getWord8
  ctrVal <- getWord8
  pure $ ControlPacket ctrId ctrVal

-- | The 'getModuleControllerPacket' tries to read two 'Word8's, followed by
--   some variable number of control packets from a 'ByteString', where the
--   first to values represent the module ID and the number of following control
--   packets
getModuleControllerPacket :: Get ModuleControllerPacket
getModuleControllerPacket = do
  modId         <- getWord8
  numCtrPackets <- getWord8
  ctrPackets    <- replicateM (fromIntegral numCtrPackets) getControlPacket
  pure $ ModuleControllerPacket modId ctrPackets

-----------------------------------------------------------------------------
-- parsers for control and module packets
-----------------------------------------------------------------------------

-- | The 'parseControlPacket' function tries to parse an incomming
--   message into a 'ControlPacket'
parseControlPacket :: B.ByteString ->
                      Either String ControlPacket
parseControlPacket = fst . runGet getControlPacket

-- | The 'parseModuleControllerPacket' function tries to parse an incomming
--   message into a 'ModuleControllerPacket'
parseModuleControllerPacket :: B.ByteString ->
                               Either String ModuleControllerPacket
parseModuleControllerPacket = fst . runGet getModuleControllerPacket


-- wrap simple parser for control and module packets to throw exceptions
data MessageException =
  InvalidPacket String
  deriving (Show, Eq)

-- |
parseControlPacket' :: B.ByteString ->
                      ExceptT MessageException Identity ControlPacket
parseControlPacket' bs = 
  case parseControlPacket bs of
    Left err -> throwError $ InvalidPacket err
    Right ctrPacket  -> pure ctrPacket

-- |
parseModuleControllerPacket' ::
  B.ByteString ->
  ExceptT MessageException Identity ModuleControllerPacket
parseModuleControllerPacket' bs =
  case parseModuleControllerPacket bs of
    Left err -> throwError $ InvalidPacket err
    Right modCtrPacket  -> pure modCtrPacket


-----------------------------------------------------------------------------
-- FIXME: consider adding more robust error system, however, not sure
--        it is really necessary as really simple and keeps it fast
-----------------------------------------------------------------------------

-- | The 'packet' function produces symbolic module controller packets, which
-- originate from whereever (conceptually the Arduino), one at a time
-- via the 'Producer' interface
-- It takes a 'ByteString' representation of the message and
-- returns a symbolic form. If the incmming message does not represent a
-- a valid packet, then a 'Nothing' packet is produced
packet :: Monad m =>
          B.ByteString ->
          Producer (Maybe ModuleControllerPacket) m ()
packet = foldr (\pac _ -> yield $ Just pac)
               (yield Nothing) . parseModuleControllerPacket

