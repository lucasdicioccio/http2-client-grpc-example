{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Lib

host = "grpcb.in"
port = 9000
authority = "grpcb.in:9000"

main :: IO ()
main = do
  args <- getArgs
  let debugOrNot = ("-v" `elem` args)
  runExample debugOrNot host port authority
