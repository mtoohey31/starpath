.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: test-coverage
test-coverage:
	scripts/test-coverage.sh

.PHONY: clean
clean:
	rm -rf _build _coverage
