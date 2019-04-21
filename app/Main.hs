{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as ByteString
import System.Environment (getArgs)
import Options.Applicative
import Lib
import SimpleLib

type UseSimpleLib = Bool

data Args = Args {
    _arghost    :: !HostName
  , _argport    :: !PortNumber
  , _arguseTLS  :: !UseTlsOrNot
  , _argverbose :: !DebugOrNot
  , _arggzip    :: !CompressOrNot
  , _argSimple  :: !UseSimpleLib
  }

args :: Parser Args
args = Args
  <$> host
  <*> port
  <*> useTLS
  <*> verbose
  <*> gzip
  <*> simple
  where
     host = strOption (long "host" <> value "grpcb.in")
     port = option auto (long "port" <> value 9001)
     useTLS = flag True False (long "plain-text")
     verbose = flag False True (long "verbose")
     gzip = flag False True (long "gzip")
     simple = flag False True (long "simple")

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
    go args
      | _argSimple args = void $ runSimpleExample (_arghost args) (_argport args) (_arguseTLS args) (_arggzip args)
      | otherwise = void $ runExample $ args2params args
