{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib (
    runExample
  , Params(..)
  , UseTlsOrNot
  , DebugOrNot
  , HostName
  , PortNumber
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, replicateM_, when)
import qualified Data.ByteString.Char8 as ByteString
import Data.Default.Class (def)
import Data.Either (isRight)
import Data.ProtoLens.TextFormat (showMessage)

import Network.GRPC
import Network.HTTP2.Client
import qualified Network.HTTP2 as HTTP2
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Proto.Protos.Grpcbin
import GRPC.Protos.Grpcbin

printDone :: Show a => String -> a -> IO ()
printDone h v = print ("Done " ++ h ++ ": " ++ show v)

type DebugOrNot = Bool
type UseTlsOrNot = Bool

wrapConn :: DebugOrNot -> (Http2FrameConnection -> Http2FrameConnection)
wrapConn False conn = conn
wrapConn True conn = conn {
      _makeFrameClientStream = \sid ->
          let frameClient = (_makeFrameClientStream conn) sid
          in frameClient {
                 _sendFrames = \mkFrames -> do
                     xs <- mkFrames
                     print $ (">>> "::String, _getStreamId frameClient, map snd xs)
                     _sendFrames frameClient (pure xs)
             }
    , _serverStream =
        let
          currentServerStrean = _serverStream conn
        in
          currentServerStrean {
            _nextHeaderAndFrame = do
                hdrFrame@(hdr,_) <- _nextHeaderAndFrame currentServerStrean
                print ("<<< "::String, HTTP2.streamId hdr, hdrFrame)
                return hdrFrame
          }
    }

tlsSettings :: UseTlsOrNot -> Maybe ClientParams
tlsSettings False = Nothing
tlsSettings True = Just $ TLS.ClientParams {
          TLS.clientWantSessionResume    = Nothing
        , TLS.clientUseMaxFragmentLength = Nothing
        , TLS.clientServerIdentification = ("127.0.0.1", "")
        , TLS.clientUseServerNameIndication = True
        , TLS.clientShared               = def
        , TLS.clientHooks                = def { TLS.onServerCertificate = \_ _ _ _ -> return []
                                               }
        , TLS.clientSupported            = def { TLS.supportedCiphers = TLS.ciphersuite_default }
        , TLS.clientDebug                = def
         }

data Params = Params
  { _debugOrNot :: DebugOrNot
  , _tlsOrNot   :: UseTlsOrNot
  , _host       :: HostName
  , _port       :: PortNumber
  , _authority  :: ByteString.ByteString
  }

runExample :: Params -> IO ()
runExample (Params{..}) = do
    putStrLn "~~~connecting~~~"
    conn <- newHttp2FrameConnection _host _port (tlsSettings _tlsOrNot)
    let goAwayHandler m = putStrLn "~~~goAway~~~" >> print m
    runHttp2Client (wrapConn _debugOrNot conn) 8192 8192 [] goAwayHandler ignoreFallbackHandler $ \client -> do
        putStrLn "~~~connected~~~"
        let ifc = _incomingFlowControl client
        let ofc = _outgoingFlowControl client
        _addCredit ifc 10000000
        _ <- _updateWindow ifc
        let unaryRpc :: RPC a => a -> Input a -> IO (Either TooMuchConcurrency (RawReply (Output a)))
            unaryRpc x y = open client _authority [] (Timeout 100) x (singleRequest y)
        let handleReply _ x = print ("~~~"::String, fmap showMessage x)

        printDone "unary-rpc-index" =<< unaryRpc Grpcbin_Index (EmptyMessage def)

        printDone "unary-rpc-empty" =<< unaryRpc Grpcbin_Empty (EmptyMessage def)

        printDone "unary-rpc-err-0" =<< unaryRpc Grpcbin_SpecificError (SpecificErrorRequest 0 "kikoo" def)
        printDone "unary-rpc-err-1" =<< unaryRpc Grpcbin_SpecificError (SpecificErrorRequest 1 "kikoo" def)
        printDone "unary-rpc-err-2" =<< unaryRpc Grpcbin_SpecificError (SpecificErrorRequest 2 "kikoo" def)

        replicateM_ 50 $ do
            printDone "unary-rpc-err-random" =<< unaryRpc Grpcbin_RandomError (EmptyMessage def)

        streamServerThread <- async $ do
            printDone "stream-server" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 Grpcbin_DummyServerStream
                 (streamReply def handleReply)

        streamClientChan <- newChan
        streamClientThread <- async $ do
            printDone "stream-client" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 Grpcbin_DummyClientStream
                 (streamRequest $ do
                     threadDelay 300000
                     v <- readChan streamClientChan
                     when (isRight v) $ print "pushing" 
                     return v)

        replicateM_ 20 (writeChan streamClientChan (Right (def :: DummyMessage)))
        writeChan streamClientChan (Left StreamDone)

        wait streamServerThread
        wait streamClientThread

        putStrLn "done"
