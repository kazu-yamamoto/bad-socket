module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    let host = "localhost"
        listenPort = "9876"
        connectPort = "6789"
    _ <- forkIO $ proxy host listenPort connectPort
    client host listenPort

client :: HostName -> ServiceName -> IO ()
client host port = do
    s <- clientSocket host port
    forever $ do
        sendAll s $ BL.fromStrict $ BS.pack $ replicate 5000 170
        recv s 4096

proxy :: HostName -> ServiceName -> ServiceName -> IO ()
proxy host listenPort connectPort = do
    fromClient <- serverSocket listenPort
    toServer <- clientSocket host connectPort
    void $ forkIO $ relay toServer fromClient "s->c"
    relay fromClient toServer "c->s"

relay :: Socket -> Socket -> String -> IO ()
relay s1 s2 str = forever $ do
    msg <- recv s1 4096
    putStrLn str
    sendAll s2 $ BL.fromStrict msg

serverSocket :: ServiceName -> IO Socket
serverSocket port = do
    let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
    addr:_ <- getAddrInfo (Just hints) (Just "localhost") (Just port)
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
