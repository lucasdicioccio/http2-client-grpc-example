{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib (
    runExample
  , Params(..)
  , CompressOrNot
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

import Data.ProtoLens.Service.Types (Service(..), HasMethod, HasMethodImpl(..))

import Network.GRPC.Client
import Network.HTTP2.Client
import qualified Network.HTTP2 as HTTP2
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Proto.Protos.Grpcbin

printDone :: Show a => String -> a -> IO ()
printDone h v = print ("Done " ++ h ++ ": " ++ show v)

type DebugOrNot = Bool
type UseTlsOrNot = Bool
type CompressOrNot = Bool

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

tlsSettings :: UseTlsOrNot -> HostName -> PortNumber -> Maybe ClientParams
tlsSettings False _ _ = Nothing
tlsSettings True host port = Just $ TLS.ClientParams {
          TLS.clientWantSessionResume    = Nothing
        , TLS.clientUseMaxFragmentLength = Nothing
        , TLS.clientServerIdentification = (host, ByteString.pack $ show port)
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
  , _gzipOrNot  :: CompressOrNot
  , _host       :: HostName
  , _port       :: PortNumber
  , _authority  :: ByteString.ByteString
  } deriving Show

runExample :: Params -> IO ()
runExample params@(Params{..}) = do
    let compress = if _gzipOrNot then gzip else uncompressed
    putStrLn "~~~connecting~~~"
    conn <- newHttp2FrameConnection _host _port (tlsSettings _tlsOrNot _host _port)
    let goAwayHandler m = putStrLn "~~~goAway~~~" >> print m
    runHttp2Client (wrapConn _debugOrNot conn) 8192 8192 [] goAwayHandler ignoreFallbackHandler $ \client -> do
        putStrLn "~~~connected~~~"
        let ifc = _incomingFlowControl client
        let ofc = _outgoingFlowControl client
        _addCredit ifc 10000000
        _ <- _updateWindow ifc

        let unaryRPC :: (Service s, HasMethod s m)
                     => RPC s m
                     -> MethodInput s m
                     -> IO (Either TooMuchConcurrency (RawReply (MethodOutput s m)))
            unaryRPC x y = open x client _authority [] (Timeout 100) compress (singleRequest x compress y)

        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "index") (EmptyMessage def)
        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "empty") (EmptyMessage def)
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (SpecificErrorRequest 0 "kikoo" def)
        printDone "unary-rpc-err-1" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (SpecificErrorRequest 1 "kikoo" def)
        printDone "unary-rpc-err-1" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (SpecificErrorRequest 1 "kikoo" def)

        replicateM_ 50 $ do
            printDone "unary-rpc-err-random" =<< unaryRPC (RPC :: RPC GRPCBin "randomError") (EmptyMessage def)

        let handleReply _ x = print ("~~~"::String, fmap showMessage x)
        streamServerThread <- async $ do
            printDone "stream-server" =<< open (RPC :: RPC GRPCBin "dummyServerStream") client
                 _authority
                 []
                 (Timeout 1000)
                 compress
                 (streamReply (RPC :: RPC GRPCBin "dummyServerStream") compress def handleReply)

        streamClientChan <- newChan
        streamClientThread <- async $ do
            printDone "stream-client" =<< open (RPC :: RPC GRPCBin "dummyClientStream") client
                 _authority
                 []
                 (Timeout 1000)
                 compress
                 (streamRequest (RPC :: RPC GRPCBin "dummyClientStream") $ do
                     threadDelay 300000
                     v <- readChan streamClientChan
                     when (isRight v) $ print "pushing" 
                     return (compress, v))

        replicateM_ 20 (writeChan streamClientChan (Right (def :: DummyMessage)))
        writeChan streamClientChan (Left StreamDone)

        wait streamServerThread
        wait streamClientThread

        putStrLn "done"
