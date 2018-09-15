{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import Control.Lens
import Control.Monad (forever, replicateM_, when)
import qualified Data.ByteString.Char8 as ByteString
import Data.Either (isRight)
import Data.ProtoLens.TextFormat (showMessage)
import Data.ProtoLens.Message (defMessage)

import Data.ProtoLens.Service.Types (Service(..), HasMethod, HasMethodImpl(..))

import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.Encoding
import Network.HTTP2.Client
import qualified Network.HTTP2 as HTTP2
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Proto.Protos.Grpcbin
import Proto.Protos.Grpcbin_Fields

printDone :: Show a => String -> a -> IO ()
printDone h v = print ("Done " ++ h ++ ": " ++ show v)

type DebugOrNot = Bool
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
    let (encoding,decoding) = if _gzipOrNot then (Encoding gzip, Decoding gzip) else (Encoding uncompressed, Decoding uncompressed)
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
            unaryRPC x y = open client _authority [] (Timeout 100) encoding decoding (singleRequest x y)

        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "index") defMessage
        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "empty") defMessage
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 0 & reason .~ "kikoo")
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 1 & reason .~ "kikoo")
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 1 & reason .~ "kikoo")

        replicateM_ 50 $ do
            printDone "unary-rpc-err-random" =<< unaryRPC (RPC :: RPC GRPCBin "randomError") defMessage

        let handleReply n _ x = print ("~~~"::String, n, showMessage x) >> pure (1+n)
        streamServerThread <- async $ do
            printDone "stream-server" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 encoding
                 decoding
                 (streamReply (RPC :: RPC GRPCBin "dummyServerStream") (0::Int) defMessage handleReply)

        streamClientThread <- async $ do
            printDone "stream-client" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 encoding
                 decoding
                 (streamRequest (RPC :: RPC GRPCBin "dummyClientStream") (10::Int) $ \n -> do
                     threadDelay 300000
                     if n > 0
                     then do
                         print ("pushing" , n)
                         return (n - 1, Right (Compressed, defMessage))
                     else do
                         print ("stop pushing" , n)
                         return (n, Left StreamDone))

        wait streamServerThread
        wait streamClientThread

        bidirStreamThread <- async $ do
            let bidiloop :: Int -> IO (Int, BiDiStep GRPCBin "dummyBidirectionalStreamStream" Int )
                bidiloop n = do
                    print "bidi-loop"
                    threadDelay 300000
                    if n > 0
                    then do
                        if n `mod` 2 == 0
                        then do
                            print "bidi-loop-send"
                            return (n - 1, SendInput Compressed defMessage)
                        else do
                            print "bidi-loop-rcv"
                            return $ (n, WaitOutput
                                (\hdrs m msg -> do
                                    print ("bidi-out", hdrs, m, msg)
                                    return (m - 1))
                                (\hdrs m trls -> do
                                    print ("bidi-closed", m, trls)
                                    return m))
                    else do
                        print ("stop bidiloop", n)
                        return $ (n, Abort)
            printDone "bidir-client" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 encoding
                 decoding
                 (steppedBiDiStream (RPC :: RPC GRPCBin "dummyBidirectionalStreamStream") (10::Int) bidiloop)

        wait bidirStreamThread

        generalHandlerThread <- async $ do
            let genLoopInput n ev = case ev of
                    (Headers hdrs) -> print ("gen-bidir-hdrs", hdrs) >> pure n
                    (RecvMessage msg) -> print ("gen-bidir-msg", msg) >> pure (n-1)
                    (Trailers trls) -> print ("gen-bidir-trls", trls) >> pure 0
                    (Invalid err) -> print ("gen-bidir-err", err) >> pure 0
            let genLoopOutput 0 = print ("finalizing") >> pure (0, Finalize)
                genLoopOutput n = print ("pushing", n) >> pure (n - 1, SendMessage Compressed defMessage)
            printDone "general-client" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 encoding
                 decoding
                 (generalHandler (RPC :: RPC GRPCBin "dummyBidirectionalStreamStream") (10::Int) genLoopInput (10::Int) genLoopOutput)

        wait generalHandlerThread
        putStrLn "done"
