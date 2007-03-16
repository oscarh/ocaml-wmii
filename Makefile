
# OCaml programs for compiling
OCAMLC = ocamlc
OCAMLDEBUG = ocamlc -g
OCAMLOPT = ocamlopt
OCAMLFIND = ocamlfind
OCAMLDOC = ocamldoc
OCAMLDEP = ocamldep

# Libs to link with
REQUIRES = unix str o9p threads

# Sources
SOURCES = src/util.ml src/wmii.ml \
			plugin/tmp_tag.mli plugin/tmp_tag.ml \
			plugin/acpi.mli plugin/acpi.ml \
			plugin/gajim.mli plugin/gajim.ml \
			plugin/date.mli plugin/date.ml \
			src/wmii_conf.ml src/wmiirc.ml

INCLUDES = -I src -I plugin

# The output to create
CLIENT = wmiirc

OCAMLFLAGS=$(INCLUDES) -thread
OCAMLOPTFLAGS=$(INCLUDES) -thread 
OCAMLFINDFLAGS=-package "$(REQUIRES)"

# Automagic stuff below
#
OBJX = $(patsubst %.ml,%.cmx,$(SOURCES))

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.PHONY: all
all: $(CLIENT)
	
$(CLIENT): $(OBJX)
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFINDFLAGS) -linkpkg -o $@ \
		$^

.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) $(OCAMLFLAGS) $(OCAMLFINDFLAGS) $<

.mli.cmi:
	$(OCAMLFIND) $(OCAMLC) -c $(OCAMLOPTFLAGS) $(OCAMLFINDFLAGS) $<

.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) -c $(OCAMLOPTFLAGS) $(OCAMLFINDFLAGS) $<

.PHONY: clean
clean:
	rm -f src/*.cmx src/*.cmi src/*.o plugin/*.cmi plugin/*.o plugin/*.cmx \
		depend $(CLIENT)

depend:
	$(OCAMLDEP)  $(SOURCES) > depend

include depend
