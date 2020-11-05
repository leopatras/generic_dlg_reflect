%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql -I$(FGLDIR)/src $*

MODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))

all: $(MODULES)

clean::
	$(RM) -f *.42? aui_const.4gl ../aui_const.4gl

