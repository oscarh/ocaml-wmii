
# OCaml programs for compiling
OCAMLDEP = ocamldep
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind
OCAMLDOC = ocamldoc

# Libs to link with
REQUIRES = unix str ixp threads

# Sources
SOURCES = src/wmiirc

# The output to create
CLIENT = wmiirc

# Automagic stuff below

CLOBJ = $(patsubst %.ml,%.cmx,$(CLSRC))

all: $(CLIENT)

$(CLIENT): $(CLOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -thread -package "$(REQUIRES)" -linkpkg -o $@ $^

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) -thread -c $(INCLUDES) -package "$(REQUIRES)" \
		$<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -thread -c $(INCLUDES) -package "$(REQUIRES)" \
		$<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) -thread -c $(INCLUDES) -package "$(REQUIRES)" \
		$<

.PHONY: clean
clean:
	rm -f src/*.cmx src/*.cmi src/*.o $(CLIENT)
