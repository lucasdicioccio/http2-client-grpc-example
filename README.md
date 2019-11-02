# THIS REPO IS MOVING

NOTE: we'll be moving the code in this repository to https://github.com/haskell-grpc-native .



# http2-client-grpc-example

This is an example gRPC client for `http2-client-grpc` using the low-level
internals for when fine-tuning is required.

An higher-level method, not-demonstrated in this example, is to use the
Network.GRPC.Client.Helpers module (which still needs a bit of improvement, for
instance, the Helper module does not automatically update connection-level flow
control).

## Run it

How to build and run:

First, install `protoc`. For instance, follow `proto-lens` directions at
https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md .

Then you need to generate code in a directory name `gen` by calling `protoc`
with `proto-lens-protoc` and `http2-client-grpc` generators. A script does these step for you:

```
$ sh prepare.sh
$ stack build
$ stack exec -- http2-client-grpc-example-exe
```
