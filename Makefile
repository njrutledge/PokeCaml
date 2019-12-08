MODULES=adventure ascii author battle btlcmd command global item main moves pokemon state types
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
PKGS=yojson,ANSITerminal
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
ZIPNAME=ms3.zip
CLOC=cloc *
CLOCML=cloc *.ml *.mli
CLOCZIP=cloc $(ZIPNAME)

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' -tag 'debug' $(TEST) && ./$(TEST) -runner sequential
	#$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

cloc:
	$(CLOC)

clocml:
	$(CLOCML)

cloczip:
	$(CLOCZIP)

zip:
	zip $(ZIPNAME) *.ml* *.json *txt _tags Makefile

zipcheck:
	zipinfo -1 $(ZIPNAME)

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

bisectclean: 
	rm -rf bisect*.out
	rm -rf report

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private $(ZIPNAME)
	rm -rf _build