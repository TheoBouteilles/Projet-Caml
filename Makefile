# Makefile

# User variables
SOURCES = math.ml vector2D.ml rectangle.ml parser.ml aircraft.ml simu.ml
TARGET = main
OCAMLC = ocamlc -g
DEP = ocamldep
OBJS = $(SOURCES:.ml=.cmo)

# Generic
all : .depend byte

byte: $(TARGET)

$(TARGET): $(OBJS)
	$(OCAMLC) -o $@ $^

%.cmi: %.mli
	ocamlc $<

%.cmo: %.ml
	ocamlc -c $<

.PHONY: clean

clean:
	rm -f *.cm[io] *~

.depend: $(SOURCES)
	ocamldep *.mli *.ml > .depend

include .depend
