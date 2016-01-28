import System.IO
import System.Environment
import System.Exit

import System.MIDI
import System.MIDI.Utility

import System.Timer.Updatable

import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as S

import Protocol.Message
import Protocol.Packet
import Control.Monad.Trans.State
import Pipes

output_channel = 1

handle :: Producer B.ByteString SerialPort ()
handle = do
  let port = "/dev/cu.wchusbserial1410"
  s <- lift $ connectController port S.CS9600 --S.CS57600
  (message >> lift disconnectController)

--loop :: Producer B.ByteString SerialPort ()
loop :: Effect SerialPort ()
loop = for handle (packet ~> lift . liftIO . print)

main :: IO ()
main = do
  evalStateT (runEffect loop) undefined

{-
main2 :: IO ()
main2 = do
  dst <- selectOutputDevice "please select an output device" Nothing
  outconn <- openDestination dst

  send outconn $ MidiMessage output_channel $ NoteOn  49 100

  t <- parallel (send outconn $ MidiMessage output_channel $ NoteOff 49 0 ) $ 100 * 1000
  waitIO t

  close outconn
  putStrLn "exit"   
-}
