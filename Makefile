%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -M $*

MODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))
FORMS   = $(patsubst %.per, %.42f, $(wildcard *.per))

all: $(FORMS) $(MODULES)


$(FORMS) $(MODULES): stores.sch

run: stores.dbs $(MODULES) $(FORMS)
	fglrun customers

customers.42m: sDAdyn.42m

test: stores.dbs $(MODULES)
	fglrun test

sql2array: stores.dbs sql2array.42m 
	fglrun sql2array

stores.dbs:
	fglcomp mkstores && fglrun mkstores

stores.sch: stores.dbs
	fgldbsch -dv sqlite -of stores -db stores.dbs

clean::
	$(RM) -f stores.dbs stores.sch
	$(RM) -f *.42?

