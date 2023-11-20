{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleLib where

import Control.Exception
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ProtoLens.Message (defMessage)
import GHC.Int (Int32)

import Network.HTTP2.Client
import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Network.GRPC.HTTP2.ProtoLens (RPC (..))
import Proto.Protos.Grpcbin
import Proto.Protos.Grpcbin_Fields

mkClient host port tlsEnabled doCompress =
    setupGrpcClient ((grpcClientConfigSimple host port tlsEnabled) { _grpcClientConfigCompression = compression })
  where
    compression = if doCompress then gzip else uncompressed

runSimpleExample host port tlsEnabled doCompress = runClientIO $ do
  grpc <- mkClient host port tlsEnabled doCompress
  -- unary
  ret <- rawUnary (RPC :: RPC GRPCBin "dummyUnary") grpc (
      defMessage
          & fInt32s .~ [1..10000]
          & fStrings .~ ["hello", "world"]
    )
  let echoedStrings = ret ^? unaryOutput . fStrings
  let echoedInt32sLen = ret ^? unaryOutput . fInt32s . to length
  printIO (echoedStrings, echoedInt32sLen)
  printIO ret

  -- general handler
  let handleIn :: Char -> IncomingEvent DummyMessage Char -> ClientIO Char
      handleIn c (Headers hdrs) = printIO (c, "headers", hdrs) >> pure (succ c)
      handleIn c (Trailers trls) = printIO (c, "trailers", trls) >> pure (succ c)
      handleIn c (RecvMessage msg) = printIO (c, msg ^. fStrings, msg ^. fInt32) >> pure (succ c)
      handleIn c (Invalid err) = liftIO $ throwIO err

  let nextOut :: Int32 -> ClientIO (Int32, OutgoingEvent DummyMessage Int32)
      nextOut 0 = pure(0, Finalize)
      nextOut n = pure(n - 1, SendMessage Uncompressed $ defMessage & fStrings .~ ["hello", "world"] & fInt32 .~ n)
  ret <- rawGeneralStream (RPC :: RPC GRPCBin "dummyBidirectionalStreamStream") grpc 'a' handleIn 30 nextOut 
  printIO ret

  where
    printIO :: Show a => a -> ClientIO ()
    printIO = liftIO . print

-- | Prism helper to unpack an unary gRPC call output.
--
-- @ out <- rawUnary rpc grpc method
--   print $ out ^? unaryOutput . somefield
-- @
unaryOutput
  :: (Applicative f, Field3 a1 b1 (Either c1 a2) (Either c1 b2)) =>
     (a2 -> f b2)
     -> Either c2 (Either c3 a1) -> f (Either c2 (Either c3 b1))
unaryOutput = _Right . _Right . _3 . _Right
