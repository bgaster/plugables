import System.IO
import System.Environment
import System.Exit

import System.MIDI
import System.MIDI.Utility

import System.Timer.Updatable

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport as S


output_channel = 1

getInSync :: SerialPort -> IO ()
getInSync s = do
  S.recv s 1 >>= \v -> if v == (B.pack "A")
                       then do putStrLn "got an A"
                               S.send s $ B.pack "B"
                               S.flush s
                               return ()
                       else getInSync s

                            
readPacket :: SerialPort -> IO String
readPacket s = fooAux "" >>= return . reverse
  where fooAux str = do
               v <- S.recv s 1
               if (B.length v == 0)
               then   fooAux str
               else if (B.head v) == '\n'
                    then return str
                    else fooAux ((B.head v) : str)


readPackets :: SerialPort -> IO ()
readPackets s = do
  str <- readPacket s
  putStrLn str
  readPackets s

main :: IO ()
main = do
  let port = "/dev/tty.usbmodem1411"  
  s <- S.openSerial port S.defaultSerialSettings { commSpeed = CS57600 }
  getInSync s
--  str <- readPacket s
--  putStrLn str
  readPackets s
  S.closeSerial s

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
