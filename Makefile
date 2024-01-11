.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: test-coverage
test-coverage:
	dune runtest --force --instrument-with bisect_ppx
	bisect-ppx-report html
	bisect-ppx-report summary

.PHONY: install-publish
install-publish:
	env -u OCAMLFIND_DESTDIR opam install --yes opam-publish

.PHONY: publish
publish:
	opam publish

.PHONY: clean
clean:
	rm -rf _build _coverage
