export FGLPROFILE=fglprofile
%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql -I$(FGLDIR)/src $*

ALLMODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))
MODULES=$(filter-out utils.42m,$(ALLMODULES))
FORMS   = $(patsubst %.per, %.42f, $(wildcard *.per))

all: $(FORMS) utils.42m $(MODULES)

$(FORMS) $(MODULES): stores.sch cols_customer.4gl

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

stores.sch: utils.42m
	stores/mkstores

utils.42m: aui_const.4gl

clean::
	$(MAKE) -C stores clean
	$(MAKE) -C tools clean
	$(RM) -f stores.dbs stores.sch
	$(RM) -f *.42? cols_customer.4gl aui_const.4gl

echo:
	@echo "MODULES:$(MODULES)"
	@echo "ALLMODULES:$(ALLMODULES)"

