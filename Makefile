export FGLPROFILE=fglprofile
%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql -I$(FGLDIR)/src $*

MODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))
FORMS   = $(patsubst %.per, %.42f, $(wildcard *.per))

all: $(FORMS) $(MODULES)


$(FORMS) $(MODULES): cols_customer.4gl aui_const.4gl stores.sch

run: stores.sch $(MODULES) $(FORMS)
	fglrun customers

test.42m: utils.42m

cols_customer.4gl: stores.sch
	tools/gen_col_names stores.sch customer cols_customer.4gl

aui_const.4gl:
	tools/gen_aui_const

sDAdyn.42m: utils.42m sql2array.42m

customers.42m: sDAdyn.42m utils.42m

orders.42m: sDAdyn.42m utils.42m

test: stores.sch $(MODULES)
	fglrun test

sql2array: stores.sch sql2array.42m utils.42m
	fglrun sql2array

stores.sch:
	fglcomp -M mkstores && fglrun mkstores

clean::
	$(MAKE) -C tools
	$(RM) -f stores.dbs stores.sch
	$(RM) -f *.42? cols_customer.4gl aui_const.4gl

