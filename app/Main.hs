module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    let host = "localhost"
        listenPort = "9876"
        connectPort = "6789"
    fromClient <- myserve host listenPort
    toServer <- myconnect host connectPort
    void $ forkIO $ proxy toServer fromClient "s->c"
    proxy fromClient toServer "c->s"
  where
    proxy s1 s2 str = forever $ do
      payload <- recv s1 4096
      putStrLn str
      sendAll s2 $ BL.fromStrict payload

myhints :: AddrInfo
myhints = defaultHints { addrSocketType = Stream }

myserve :: HostName -> ServiceName -> IO Socket
myserve host port = do
  (addr:_) <- getAddrInfo (Just myhints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1
  fst <$> accept sock

myconnect :: HostName -> ServiceName -> IO Socket
myconnect host port = do
  (addr:_) <- getAddrInfo (Just myhints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock
