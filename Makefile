 .PHONY: bench
bench:
	for i in $(shell seq 1 10); do dune exec test-bechamel/fact.exe; done
