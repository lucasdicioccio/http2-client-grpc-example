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

import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Control.Concurrent.Lifted (threadDelay)
import Control.Lens
import Control.Monad (forever, replicateM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as ByteString
import Data.Either (isRight)
import Data.ProtoLens.TextFormat (showMessage)
import Data.ProtoLens.Message (defMessage)

import Data.ProtoLens.Service.Types (Service(..), HasMethod, HasMethodImpl(..))

import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.Encoding
import Network.GRPC.HTTP2.ProtoLens (RPC (..))
import Network.HTTP2.Client
import qualified Network.HTTP2 as HTTP2
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Proto.Protos.Grpcbin
import Proto.Protos.Grpcbin_Fields

printDone :: Show a => String -> a -> ClientIO ()
printDone h v = printIO ("Done " ++ h ++ ": " ++ show v)

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print

putStrLnIO :: MonadIO m => String -> m ()
putStrLnIO = liftIO . putStrLn

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
                     printIO $ (">>> "::String, _getStreamId frameClient, map snd xs)
                     _sendFrames frameClient (pure xs)
             }
    , _serverStream =
        let
          currentServerStrean = _serverStream conn
        in
          currentServerStrean {
            _nextHeaderAndFrame = do
                hdrFrame@(hdr,_) <- _nextHeaderAndFrame currentServerStrean
                printIO ("<<< "::String, HTTP2.streamId hdr, hdrFrame)
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

runExample :: Params -> IO (Either ClientError ())
runExample params@(Params{..}) = runClientIO $ do
    let (encoding,decoding) = if _gzipOrNot then (Encoding gzip, Decoding gzip) else (Encoding uncompressed, Decoding uncompressed)
    putStrLnIO "~~~connecting~~~"
    conn <- newHttp2FrameConnection _host _port (tlsSettings _tlsOrNot _host _port)
    let goAwayHandler m = putStrLnIO "~~~goAway~~~" >> printIO m
    runHttp2Client (wrapConn _debugOrNot conn) 8192 8192 [] goAwayHandler ignoreFallbackHandler $ \client -> do
        putStrLnIO "~~~connected~~~"
        let ifc = _incomingFlowControl client
        let ofc = _outgoingFlowControl client
        liftIO $ _addCredit ifc 10000000
        _ <- _updateWindow ifc

        let unaryRPC :: (Service s, HasMethod s m)
                     => RPC s m
                     -> MethodInput s m
                     -> ClientIO (Either TooMuchConcurrency (RawReply (MethodOutput s m)))
            unaryRPC x y = open client _authority [] (Timeout 100) encoding decoding (singleRequest x y)

        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "index") defMessage
        printDone "unary-rpc-index" =<< unaryRPC (RPC :: RPC GRPCBin "empty") defMessage
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 0 & reason .~ "kikoo")
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 1 & reason .~ "kikoo")
        printDone "unary-rpc-err-0" =<< unaryRPC (RPC :: RPC GRPCBin "specificError") (defMessage & code .~ 1 & reason .~ "kikoo")

        replicateM_ 50 $ do
            printDone "unary-rpc-err-random" =<< unaryRPC (RPC :: RPC GRPCBin "randomError") defMessage

        let handleReply n _ x = printIO ("~~~"::String, n, showMessage x) >> pure (1+n)
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
                         printIO ("pushing" , n)
                         return (n - 1, Right (Compressed, defMessage))
                     else do
                         printIO ("stop pushing" , n)
                         return (n, Left StreamDone))

        wait streamServerThread
        wait streamClientThread

        bidirStreamThread <- async $ do
            let bidiloop :: Int -> ClientIO (Int, BiDiStep DummyMessage DummyMessage Int )
                bidiloop n = do
                    printIO "bidi-loop"
                    threadDelay 300000
                    if n > 0
                    then do
                        if n `mod` 2 == 0
                        then do
                            printIO "bidi-loop-send"
                            return (n - 1, SendInput Compressed defMessage)
                        else do
                            printIO "bidi-loop-rcv"
                            return $ (n, WaitOutput
                                (\hdrs m msg -> do
                                    printIO ("bidi-out", hdrs, m, msg)
                                    return (m - 1))
                                (\hdrs m trls -> do
                                    printIO ("bidi-closed", m, trls)
                                    return m))
                    else do
                        printIO ("stop bidiloop", n)
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
                    (Headers hdrs) -> printIO ("gen-bidir-hdrs", hdrs) >> pure n
                    (RecvMessage msg) -> printIO ("gen-bidir-msg", msg) >> pure (n-1)
                    (Trailers trls) -> printIO ("gen-bidir-trls", trls) >> pure 0
                    (Invalid err) -> printIO ("gen-bidir-err", err) >> pure 0
            let genLoopOutput 0 = printIO ("finalizing") >> pure (0, Finalize)
                genLoopOutput n = printIO ("pushing", n) >> pure (n - 1, SendMessage Compressed defMessage)
            printDone "general-client" =<< open client
                 _authority
                 []
                 (Timeout 1000)
                 encoding
                 decoding
                 (generalHandler (RPC :: RPC GRPCBin "dummyBidirectionalStreamStream") (10::Int) genLoopInput (10::Int) genLoopOutput)

        wait generalHandlerThread
        putStrLnIO "done"
