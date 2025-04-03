build:
	dune build
test:
	dune runtest
clean: 
	dune clean
.PHONY: test clean build