.PHONY: all build clean test fmt lock

all: build

build:
	dune build

clean:
	dune clean

test:
	dune test

fmt:
	dune fmt

lock:
	dune pkg lock
