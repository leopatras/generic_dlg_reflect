.PRECIOUS: %.4gl
export FGLPROFILE=fglprofile
%.42f: %.per 
	fglform -M $<

%.42m: %.4gl 
	fglcomp -r -M -Wall -Wno-stdsql $*

ALLMODULES = $(patsubst %.4gl, %.42m, $(wildcard *.4gl))
MODULES=$(filter-out utils.42m,$(ALLMODULES))
ALLFORMS   = $(patsubst %.per, %.42f, $(wildcard *.per))
FORMS=$(filter-out customers.42f,$(ALLFORMS))

all: stores.sch customers.42f cols_customer.4gl $(FORMS) utils.42m $(MODULES)

$(FORMS) $(MODULES): stores.sch cols_customer.4gl

run: all stores.sch $(MODULES) $(FORMS)
	fglrun stores.42m

crun: all stores.sch $(MODULES) $(FORMS)
	fglrun cstores.42m

test.42m: utils.42m

cols_customer.4gl: customers.42f
	#tools/gen_col_names stores.sch customer cols_customer.4gl
	tools/gen_form_names customers.42f cols_customer.4gl

aui_const.4gl:
	tools/gen_aui_const

sDAdyn.42m: utils.42m sql2array.42m

customers.42m: sDAdyn.42m utils.42m customers.42f customers_singlerow.42f

cust_ex.42m: cust_ex.4gl sDAdyn.42m utils.42m
#by intention we ignore column name warnings to provoke an SQL error at runtime
	fglcomp -Wall -Wno-stdsql -Wno-colname cust_ex.4gl

customers: customers.42m
	fglrun $@

ccustomers.42m: utils.42m

ccustomers: ccustomers.42m
	fglrun $@

corders.42m: utils.42m

corders: corders.42m
	fglrun $@

citems.42m: utils.42m items.42f

citems: citems.42m
	fglrun $@

items.42m: utils.42m

items: items.42m
	fglrun $@

orders.42m: sDAdyn.42m utils.42m

orders: orders.42m orders.42f
	fglrun orders

test: stores.sch $(MODULES)
	fglrun test

sql2array: stores.sch sql2array.42m utils.42m
	fglrun sql2array


stores.sch: utils.42m
	stores/mkstores

utils.42m: aui_const.4gl

format: 
	tools/fglformat
	make -C tools format
	make -C stores format

clean::
	$(MAKE) -C stores clean
	$(MAKE) -C tools clean
	$(RM) -f stores.dbs stores.sch
	$(RM) -f *.42? *.4gl~ cols_customer.4gl aui_const.4gl *.fgldeb

echo:
	@echo "ALLMODULES:$(ALLMODULES)"
	@echo "MODULES:$(MODULES)"
	@echo "ALLFORMS:$(ALLFORMS)"
	@echo "FORMS:$(FORMS)"

