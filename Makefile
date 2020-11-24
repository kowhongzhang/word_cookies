MODULES= game level interface authors addons
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
INTERFACE=interface.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS = oUnit,yojson,ANSITerminal

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)


zip:
	zip -r ms2src.zip *.ml* _tags README.md LOC.txt install.txt extractor Makefile

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(INTERFACE) && ./$(INTERFACE)

clean:
	ocamlbuild -clean

docs: docs-public docs-private

cleandocs: 	
	rm -r doc.public
	rm -r doc.private

rmzip:
	rm *.zip
