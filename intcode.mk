ICVM_TYPE ?= c
DEBUG ?=

ICDIR ?= $(abspath xzintbit)

ifeq ($(shell test -d $(ICDIR) || echo error),error)
	$(error ICDIR variable is invalid; point it where https://github.com/matushorvath/xzintbit is built)
endif

ICBINDIR ?= $(abspath $(ICDIR)/bin)
ICVMSDIR ?= $(abspath $(ICDIR)/vms)

ICAS_NAME = $(if $(DEBUG),asd,as)

ICVM ?= $(abspath $(ICVMSDIR)/$(ICVM_TYPE)/ic)
ICAS ?= $(abspath $(ICBINDIR)/$(ICAS_NAME).input)
ICBIN2OBJ ?= $(abspath $(ICBINDIR)/bin2obj.input)
ICLD ?= $(abspath $(ICBINDIR)/ld.input)
ICLDMAP ?= $(abspath $(ICBINDIR)/ldmap.input)
LIBXIB ?= $(abspath $(ICBINDIR)/libxib.a)

IC_ERROR_RESULT ?= false

define run-intcode-as
	cat $(filter-out $<,$^) $< | $(ICVM) $(ICAS) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-ar
	cat $^ | sed 's/^.C$$/.L/g' > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-ld
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
	echo .$$ | cat $^ - | $(ICVM) $(ICLDMAP) > $@.map.yaml || ( cat $@.map.yaml ; $(IC_ERROR_RESULT) )
endef

define run-intcode-bin2obj
	wc -c $< | sed 's/$$/$(if $(BIN2OBJ_NAME),\/$(BIN2OBJ_NAME),)/' | cat - $< | $(ICVM) $(ICBIN2OBJ) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-vm
	$(ICVM) $< > $@ $(if $(word 2,$^),< $(word 2,$^)) || ( cat $@ ; $(IC_ERROR_RESULT) )
endef
