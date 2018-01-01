# http2-client-grpc-example

This is an example gRPC client for `http2-client-grpc`.

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
