{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as ByteString
import System.Environment (getArgs)
import Options.Applicative
import Lib

data Args = Args {
    _arghost    :: !HostName
  , _argport    :: !PortNumber
  , _arguseTLS  :: !UseTlsOrNot
  , _argverbose :: !DebugOrNot
  , _arggzip    :: !CompressOrNot
  }

args :: Parser Args
args = Args
  <$> host
  <*> port
  <*> useTLS
  <*> verbose
  <*> gzip
  where
     host = strOption (long "host" <> value "grpcb.in")
     port = option auto (long "port" <> value 9001)
     useTLS = flag True False (long "plain-text")
     verbose = flag False True (long "verbose")
     gzip = flag False True (long "gzip")

args2params :: Args -> Params
args2params args = Params
  (_argverbose args)
  (_arguseTLS args)
  (_arggzip args)
  (_arghost args)
  (_argport args)
  (ByteString.pack $ _arghost args <> ":" <> show (_argport args))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (helper <*> args) (mconcat [
        fullDesc
      , header "http2-client-grpc-exe: a demo GRPC client"
      ])
    go = runExample . args2params
