
# OCaml programs for compiling
OCAMLDEP = ocamldep
OCAMLC = ocamlc
OCAMLDEBUG = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind
OCAMLDOC = ocamldoc

# Libs to link with
REQUIRES = unix str ixp threads

# Sources
SOURCES = src/wmii.ml src/wmii_conf.ml src/wmiirc.ml

# The output to create
CLIENT = wmiirc

INCLUDES = -I src

# Automagic stuff below

CLOBJ = $(patsubst %.ml,%.cmx,$(SOURCES))

all: $(CLIENT)

debug: $(BYTECODE)

$(CLIENT): $(CLOBJ)
	$(OCAMLFIND) $(OCAMLOPT) -thread -package "$(REQUIRES)" -linkpkg -o $@ \
		$^

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -thread -c $(INCLUDES) -package "$(REQUIRES)" \
		$<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) -thread -c $(INCLUDES) -package "$(REQUIRES)" \
		$<

.PHONY: clean
clean:
	rm -f src/*.cmx src/*.cmi src/*.o $(CLIENT)
