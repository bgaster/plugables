{-|
Module      : AppState
Description : Some global type definitions for application
Copyright   : (c) Benedict R. Gaster, 2016
License     : BSD3
Maintainer  : benedict.gaster@email.com
Stability   : experimental
Portability : Not sure!

Defines some basic types used in different modules and also a state monad,
which is used in the Arudino to MIDI pipeline.
-}

module AppState (
  ControlID,
  ControlValue,
  ModuleID,
  MidiChannel,
  ControlCommand,
  ControllerOffset,
  ModuleTable,
  ControllerTable,
  Tables,
  AppState,
  AppM,
  emptyAppState,
  getPort,
  putPort,
  getMidi,
  putMidi,
  getModuleTable,
  putModuleTable,
  getControllerTable,
  putControllerTable,
  getTables,
  putTables
  ) where

import System.Hardware.Serialport
import System.MIDI

import Data.Word
import Control.Monad.Trans.State

import qualified Data.Vector.Unboxed as V

-- FIXME: we define this here to avoid circular dependencies, we should probably move it
-- to AppTypes
type ControlID        = Word8
type ControlValue     =  Word8
type ModuleID         = Word8
type MidiChannel      = Int
type ControlCommand   = Int
type ControllerOffset = Int

type ModuleTable      = V.Vector (MidiChannel, ControllerOffset)
type ControllerTable  = V.Vector ControllerOffset

type Tables = (ModuleTable, ControllerTable)

--------------------------------------------------------------------------------

-- | The 'State' type is the type of values stored in our AppMonad
data AppState = AppState {
  -- | The 'port' that is being used to communicate with the Arduino
  port :: SerialPort,
  -- |
  midi :: Connection,
  moduleTable :: ModuleTable,
  controllerTable :: ControllerTable }

-- | The 'AppM' type is a state monad (with IO support) used to control 
--   access/communication to the Arduino hardware (serial port) and
--   output midi device
type AppM = StateT AppState IO

-- | Initializer for empty state, used to start pipeline
emptyAppState :: AppState
emptyAppState = AppState undefined undefined undefined undefined

-- | function 'getPort' retrives the serial port
getPort :: AppM SerialPort
getPort = get >>= pure . port 

-- | function 'getPort' stores the serial port
putPort :: SerialPort -> AppM ()
putPort p = get >>= \s -> put $ s { port = p }

-- | function 'getMidi' retrives the ouput MIDI device
getMidi :: AppM Connection
getMidi = get >>= pure . midi

-- | function 'putMidi' stores output MIDI device
putMidi :: Connection -> AppM ()
putMidi c = get >>= \s -> put $ s { midi = c }

-- | function 'getModuleTable' retrives the module lookup table
getModuleTable :: AppM (V.Vector (MidiChannel, ControllerOffset))
getModuleTable = get >>= pure . moduleTable

-- | function 'putModuleTable' stores module lookup table
putModuleTable :: V.Vector (MidiChannel, ControllerOffset) -> AppM ()
putModuleTable mt = get >>= \s -> put $ s { moduleTable = mt }

-- | function 'getControllerTable' retrives the controller lookup table
getControllerTable :: AppM ControllerTable
getControllerTable = get >>= pure . controllerTable

-- | function 'putControllerTable' stores controller lookup table
putControllerTable :: ControllerTable -> AppM ()
putControllerTable ct = get >>= \s -> put $ s { controllerTable = ct }

-- | function 'getTables' retrives the module and controller lookup tables
getTables :: AppM Tables
getTables = get >>= \st -> pure $ (moduleTable st, controllerTable st)

-- | function 'putTables' stores module and controller lookup tables
putTables :: Tables -> AppM ()
putTables tabs = get >>= \s -> put $ s { controllerTable = snd tabs,
                                         moduleTable = fst tabs }

