export FGLPROFILE=fglprofile
%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql -I$(FGLDIR)/src $*

MODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))
FORMS   = $(patsubst %.per, %.42f, $(wildcard *.per))

all: $(FORMS) wrap_dbslib.42m $(MODULES)


$(FORMS) $(MODULES): stores.sch

run: stores.sch $(MODULES) $(FORMS)
	fglrun customers

test.42m: utils.42m

sDAdyn.42m: utils.42m sql2array.42m

customers.42m: sDAdyn.42m utils.42m

wrap_dbslib.42m: wrap_dbslib.4gl
	fglcomp -r -M -Wno-stdsql -I $(FGLDIR)/src wrap_dbslib

orders.42m: sDAdyn.42m utils.42m

test: stores.sch $(MODULES)
	fglrun test

sql2array: stores.sch wrap_dbslib.42m sql2array.42m utils.42m
	fglrun sql2array

stores.sch:
	fglcomp -M mkstores && fglrun mkstores

clean::
	$(RM) -f stores.dbs stores.sch
	$(RM) -f *.42?

