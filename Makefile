all: build test check-fmt

build:
	dune build

test:
	dune test

check-fmt:
	dune build @fmt

promote:
	dune promote
