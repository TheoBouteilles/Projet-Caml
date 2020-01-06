# Makefile

# User variables

# Generic
%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml
	ocamlc -c $<

#.PHONY: clean

#clean:
#	rm -f *.cm[io] *~

.depend: $(SOURCES)
	ocamldep *.mli *.ml > .depend

include .depend
