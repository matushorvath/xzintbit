ICVM_TYPE ?= c

ICDIR ?= $(abspath xzintbit)

ifeq ($(shell test -d $(ICDIR) || echo error),error)
	$(error ICDIR variable is invalid; point it where https://github.com/matushorvath/xzintbit is built)
endif

ICBINDIR ?= $(abspath $(ICDIR)/bin)
ICVMSDIR ?= $(abspath $(ICDIR)/vms)

ICVM ?= $(abspath $(ICVMSDIR)/$(ICVM_TYPE)/ic)
ICVM_AS ?= $(ICVM)
ICVM_LD ?= $(ICVM)
ICVM_BIN2OBJ ?= $(ICVM)
ICVM_VM ?= $(ICVM)

ICAS ?= $(abspath $(ICBINDIR)/as.input)
ICBIN2OBJ ?= $(abspath $(ICBINDIR)/bin2obj.input)
ICLD ?= $(abspath $(ICBINDIR)/ld.input)
ICLDMAP ?= $(abspath $(ICBINDIR)/ldmap.input)
LIBXIB ?= $(abspath $(ICBINDIR)/libxib.a)

IC_ERROR_RESULT ?= false

define run-intcode-as
	cat $(filter-out $<,$^) $< | $(ICVM_AS) $(ICAS) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-ar
	cat $^ | sed 's/^.C$$/.L/g' > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-ld
	echo .$$ | cat $^ - | $(ICVM_LD) $(ICLD) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
	echo .$$ | cat $^ - | $(ICVM_LD) $(ICLDMAP) > $@.map.yaml || ( cat $@.map.yaml ; $(IC_ERROR_RESULT) )
endef

define run-intcode-bin2obj
	wc -c $< | sed 's/$$/$(if $(BIN2OBJ_NAME),\/$(BIN2OBJ_NAME),)/' | cat - $< | $(ICVM_BIN2OBJ) $(ICBIN2OBJ) > $@ || ( cat $@ ; $(IC_ERROR_RESULT) )
endef

define run-intcode-vm
	$(ICVM_VM) $< > $@ $(if $(ICVM_STDERR),2> $(ICVM_STDERR)) $(if $(word 2,$^),< $(word 2,$^)) || ( cat $@ ; $(IC_ERROR_RESULT) )
endef
