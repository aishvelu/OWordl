.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f final-project.zip
	zip -r final-project.zip . -x@exclude.lst

clean:
	dune clean
	rm -f final-project.zip

docs:
	dune build @doc
