module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    let localhost = "localhost"
        listenPort = "9876"
        connectPort = "6789"
    proxy localhost listenPort connectPort

proxy :: HostName -> ServiceName -> ServiceName -> IO ()
proxy localhost listenPort connectPort = do
    fromClient <- serverSocket localhost listenPort
    toServer <- clientSocket localhost connectPort
    void $ forkIO $ relay toServer fromClient "s->c"
    relay fromClient toServer "c->s"

relay :: Socket -> Socket -> String -> IO ()
relay s1 s2 str = forever $ do
    payload <- recv s1 4096
    putStrLn str
    sendAll s2 $ BL.fromStrict payload

serverSocket :: HostName -> ServiceName -> IO Socket
serverSocket host port = do
    let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 1
    fst <$> accept sock

clientSocket :: HostName -> ServiceName -> IO Socket
clientSocket host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
