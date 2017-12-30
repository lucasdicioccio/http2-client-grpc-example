#!/bin/bash

mkdir -p gen
mkdir -p gen-bin
mkdir -p protos

curl 'https://raw.githubusercontent.com/moul/pb/master/grpcbin/grpcbin.proto' > protos/grpcbin.proto

stack install --local-bin-path=gen-bin proto-lens-protoc
stack install --local-bin-path=gen-bin http2-client-grpc

protolens="`pwd`/gen-bin/proto-lens-protoc"
grpc="`pwd`/gen-bin/http2-client-grpc-gen"

if [ -x "${protolens}" ]
then
	echo "using ${protolens}" ;
else
	echo "no proto-lens-protoc"
	exit 2
fi;

if [ -x "${grpc}" ]
then
	echo "using ${grpc}"
else
	echo "no http2-client-grpc-gen"
	exit 2
fi;

protoc  "--plugin=protoc-gen-haskell-protolens=${protolens}" \
	"--plugin=protoc-gen-haskell-grpc=${grpc}" \
	--haskell-protolens_out=./gen \
	--haskell-grpc_out=./gen \
	./protos/grpcbin.proto

echo "# Generated modules:"
find gen -name "*.hs" | sed -e 's/gen\///' | sed -e 's/\.hs$//' | tr '/' '.'
