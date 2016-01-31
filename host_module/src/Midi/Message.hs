{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Midi.Message
Description : Producer interface for generating midi messages from control packets
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@email.com
Stability   : experimental
Portability : Not sure!

We use two lookup tables for mapping messages from the Arduino to MIDI. As we
don't want the user to have to recompile the Haskell code, we use the web-app
to generate the data for the tables, see below, and then parse this and create
the lookup tables at runtime.

Module Table

A table consisting of two entries:

	channel: MIDI channel for module

        controller offset: offset, into the controller table (see below), to
                   the first controller for module. It is normalized to the interval:

                    [0,C)

                   where C is total number of controllers for all modules in the
                   instrument Effectively this table is a function from

	moduleID -> (channel, controller offset)

moduleID is normalized for an instrument, where N is the number of
modules in a instrument, to the interval:

	[0,N)
	
Controller Table

A table consisting of 1 entry:

	control command: MIDI control command value, used in sending
                         MIDI control messages for controller

Effectively this table is a function:

	controllerOffset -> control command
	
Text representation of tables

 To avoid having to update the Haskell code for the host app for every
instrument, we instead define a very simple grammar for describing the
lookup table data. We generate the table data from JS in the
instrument control app and load this data into two lookup tables in
the Haskell host app.

As we are automatically generating the tables, along with the Arduino
code that generates messages with values used to access these tables,
we can assume that table access is well-defined and thus we use
unsafeindex and other operations that are not safe, in general. Of
course, if these invariant are not kept to, then it is likely to cause
the host application to crash

Grammar

	nat := [0-9]+

	channel_offset ::= '(' nat, nat ')'
	control_command ::= nat
	
	moduleID_table_list ::= channel_offset
	                      | channel_offset ',' moduleID_table_list
	
	control_command_list  ::= control_command
	                        | control_command ',' control_command_list
	
	tables ::= '[' channel_offset_table_list  ']' '[' control_command_list ']'
-}

module Midi.Message(
  connectDevice,
  midi) where

import System.IO

import qualified Data.ByteString.Char8 as B

import System.MIDI
import System.MIDI.Utility

import Protocol.Packet
import Control.Monad.Trans.State
import Control.Monad
import Pipes

import qualified Data.Vector.Unboxed as V

import Data.Char
import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Number

import AppState

--------------------------------------------------------------------------------
-- Parser for lookup table data
--------------------------------------------------------------------------------

type Parse a = CharParser () a

whitespace :: Parse ()
whitespace = (spaces >>= \_ -> pure ()) <|> (newline >>= \_ -> pure ())

parseList :: Parse a -> Parse [a]
parseList p = do
  char '['
  whitespace
  xs <- p  `sepBy` (do whitespace; char ','; whitespace)
  whitespace
  char ']'
  pure xs

parseChannelOffset :: Parse (MidiChannel, ControllerOffset)
parseChannelOffset = do
  char '('
  whitespace
  channel <- nat
  whitespace
  char ','
  whitespace
  offset <- nat
  whitespace
  char ')'
  pure (channel, offset)

parseModuleTable :: Parse [(MidiChannel, ControllerOffset)]
parseModuleTable = parseList parseChannelOffset

parseControllerTable :: Parse [ControllerOffset]
parseControllerTable = parseList (do whitespace; n <- nat; whitespace; pure n)

parseTables :: Parse ([(MidiChannel, ControllerOffset)], [ControllerOffset])
parseTables = do
  mtable <- parseModuleTable
  whitespace
  ctable <- parseControllerTable
  pure (mtable, ctable)

--------------------------------------------------------------------------------
-- Functions to connect to an output MIDI device and initialize loopup tables
--------------------------------------------------------------------------------

-- FIXME: we should really just return a producer, then there is no need for
-- the tables to be part of the state monad

-- | function 'connectDevice' tries to intialize lookup tables and connect
--   to output MIDI device
connectDevice :: String -> AppM Bool
connectDevice filename = do

  -- configure MIDI device, note we ask user to pick one if there are many
  dst <- liftIO $ selectOutputDevice "please select an output device" Nothing
  outconn <- liftIO $ openDestination dst
  putMidi outconn

  -- now load tables
  handle   <- liftIO $ openFile filename ReadMode
  contents <- liftIO $ hGetContents handle

  foldr (\(mtab, ctab) _ ->
             putTables (V.fromList mtab, V.fromList ctab) >>= \_ -> pure True)
        (pure False)
        (runParser (wrap parseTables) () "" contents) 

  where
    wrap :: Parse a -> Parse a
    wrap p = do
      whitespace
      x <- p
      whitespace
      eof
      pure x

--------------------------------------------------------------------------------
-- convert control packets 
--------------------------------------------------------------------------------

-- | function 'controlPacketToMidi' converts control packets to MIDI control
--   command messages
controlPacketToMidi :: MidiChannel ->
                       ControllerTable ->
                       ControllerOffset ->
                       ControlPacket ->
                       AppM MidiMessage
controlPacketToMidi channel ctab offset cp =
  pure $ MidiMessage channel $ CC (ctab V.! (offset + fromIntegral (controlId cp)))
                           (fromIntegral $ controlValue cp)

--------------------------------------------------------------------------------
-- Module control packet consummer
--------------------------------------------------------------------------------

-- | function 'midi' consumes module control packets, processes them into
--   a sequence of MIDI messages, forwarding them onwards via the output
--   MIDI device
midi :: Consumer (Maybe ModuleControllerPacket) AppM ()
midi = do
  mpacket <- await
  case mpacket of
    Just packet -> do
      connection <- lift $ getMidi
      (modtab, contab) <- lift $ getTables
      let (channel, offset) = modtab V.! (fromIntegral $ moduleID packet)
      lift $ mapM (\cp -> do msg <- controlPacketToMidi channel contab offset cp
                             liftIO $ send connection msg)
                  (controlPackets packet)
      pure ()
    Nothing -> pure ()
  midi
  
