PACKAGES = extlib,xml-light,ocamlduce,num
FILES = schema_dtd.ml tools.ml schema.ml schema_tools.ml schema_loader.ml rand.ml fuzz.ml cmdline.ml

NAME = xmlfuzzer
VERSION := $(shell head -n 1 VERSION)
CAMLC   = ocamlducefind ocamlc -g $(LIB)
CAMLOPT = ocamlducefind ocamlopt -g $(LIB)
CAMLDOC = ocamlducefind ocamldoc $(LIB)
CAMLDEP = ocamlducefind ocamldep
LIB = -package $(PACKAGES)

OBJS    = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

BYTE = $(NAME).byte
NATIVE = $(NAME)

all: byte native

byte: $(BYTE)
native: $(NATIVE)

$(BYTE): $(OBJS)
	$(CAMLC) -linkpkg -o $@ $(OBJS)

$(NATIVE): $(OPTOBJS)
	$(CAMLOPT) -linkpkg -o $@ $(OBJS:.cmo=.cmx)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

.ml.cmo:
	$(CAMLC) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~
	-rm -f .depend
	-rm -rf doc
	-rm -f $(BYTE) $(NATIVE)

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
