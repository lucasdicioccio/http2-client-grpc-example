{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleLib where

import Control.Lens
import Data.Default.Class (def)

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
  ret <- rawUnary (RPC :: RPC GRPCBin "dummyUnary") grpc (
      def
          & fInt32s .~ [1..10000]
          & fStrings .~ ["hello", "world"]
    )
  let echoedStrings = ret ^? unaryOutput . fStrings
  let echoedInt32sLen = ret ^? unaryOutput . fInt32s . to length
  print (echoedStrings, echoedInt32sLen)
  return ret
