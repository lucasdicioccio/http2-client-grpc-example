{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleLib where

import Control.Exception
import Control.Lens
import Data.ProtoLens.Message (defMessage)
import GHC.Int (Int32)

import Network.GRPC.Client
import Network.GRPC.Client.Helpers
import Proto.Protos.Grpcbin
import Proto.Protos.Grpcbin_Fields

mkClient host port tlsEnabled doCompress =
    setupGrpcClient ((grpcClientConfigSimple host port tlsEnabled) { _grpcClientConfigCompression = compression })
  where
    compression = if doCompress then gzip else uncompressed

runSimpleExample host port tlsEnabled doCompress = do
  grpc <- mkClient host port tlsEnabled doCompress
  -- unary
  ret <- rawUnary (RPC :: RPC GRPCBin "dummyUnary") grpc (
      defMessage
          & fInt32s .~ [1..10000]
          & fStrings .~ ["hello", "world"]
    )
  let echoedStrings = ret ^? unaryOutput . fStrings
  let echoedInt32sLen = ret ^? unaryOutput . fInt32s . to length
  print (echoedStrings, echoedInt32sLen)
  print ret

  -- general handler
  let handleIn :: Char -> IncomingEvent GRPCBin "dummyBidirectionalStreamStream" Char -> IO Char
      handleIn c (Headers hdrs) = print (c, "headers", hdrs) >> pure (succ c)
      handleIn c (Trailers trls) = print (c, "trailers", trls) >> pure (succ c)
      handleIn c (RecvMessage msg) = print (c, msg ^. fStrings, msg ^. fInt32) >> pure (succ c)
      handleIn c (Invalid err) = throwIO err

  let nextOut :: Int32 -> IO (Int32, OutgoingEvent GRPCBin "dummyBidirectionalStreamStream" Int32)
      nextOut 0 = pure(0, Finalize)
      nextOut n = pure(n - 1, SendMessage Uncompressed $ defMessage & fStrings .~ ["hello", "world"] & fInt32 .~ n)
  ret <- rawGeneralStream (RPC :: RPC GRPCBin "dummyBidirectionalStreamStream") grpc 'a' handleIn 30 nextOut 
  print ret
