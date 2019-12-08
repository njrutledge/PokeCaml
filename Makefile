MODULES=ascii item adventure command state main author types moves pokemon btlcmd battle
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
CLOC=cloc *
CLOCML=cloc *.ml *.mli
CLOCZIP=cloc ms3.zip

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

cloc:
	$(CLOC)

clocml:
	$(CLOCML)

cloczip:
	$(CLOCZIP)

zip:
	zip ms3.zip *.ml* *.json *txt _tags Makefile
	
bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-bag-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adventure.zip
	rm -rf _build
