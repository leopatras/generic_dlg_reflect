.PRECIOUS: ../%.4gl 
%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql $*

MODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))

all: $(MODULES)

gen_form_names.42m: #needs utils.4gl in the dir above us
	cd .. && fglcomp -r -M -Wall -Wno-stdsql tools/gen_form_names && ln gen_form_names.42m tools/gen_form_names.42m

format:
	../tools/fglformat

clean::
	$(RM) -f *.42? *.4gl~

