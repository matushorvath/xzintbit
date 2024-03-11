ICVM_TYPE ?= c

ICVM ?= $(abspath ../../vms)/$(ICVM_TYPE)/ic
ICAS ?= $(abspath ../../bin/as.input)
ICBIN2OBJ ?= $(abspath ../../bin/bin2obj.input)
ICLD ?= $(abspath ../../bin/ld.input)

BINDIR ?= bin
OBJDIR ?= obj

ifndef TESTLOG
	TESTLOG := $(shell mktemp)
endif

NAME = $(notdir $(CURDIR))

HAVE_COLOR := $(or $(FORCE_COLOR), $(shell [ -n $$(tput colors) ] && [ $$(tput colors) -ge 8 ] && echo 1))
ifeq ($(HAVE_COLOR),1)
	COLOR_NORMAL := "$(shell tput sgr0)"
	COLOR_RED := "$(shell tput setaf 1)"
	COLOR_GREEN := "$(shell tput setaf 2)"
endif

.PHONY: default test
default: test
	[ $(MAKELEVEL) -eq 0 ] && cat $(TESTLOG) && rm -f $(TESTLOG)

.PHONY: test-prep
test-prep:
	rm -rf $(BINDIR) $(OBJDIR)
	mkdir -p $(BINDIR) $(OBJDIR)

$(BINDIR)/%.stdout: $(BINDIR)/%.input %.stdin
	printf '$(NAME): processing stdin ' >> $(TESTLOG)
	$(ICVM) $< > $@ < $(patsubst %.input,%.stdin,$(notdir $<)) || ( cat $@ ; true )
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

$(BINDIR)/%.txt: $(BINDIR)/%.input
	printf '$(NAME): executing ' >> $(TESTLOG)
	$(ICVM) $< > $@ || ( cat $@ ; true )
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

$(BINDIR)/%.input: $(OBJDIR)/%.o
	printf '$(NAME): linking ' >> $(TESTLOG)
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; true )
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

$(BINDIR)/%.a: $(OBJDIR)/%.o
	printf '$(NAME): archiving ' >> $(TESTLOG)
	echo .L | cat - $^ > $@ || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

$(OBJDIR)/%.o: %.s
	printf '$(NAME): assembling ' >> $(TESTLOG)
	cat $^ | $(ICVM) $(ICAS) > $@ || ( cat $@ ; true )
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

$(OBJDIR)/%.o: %.bin
	printf '$(NAME): running bin2obj ' >> $(TESTLOG)
	ls -n $< | awk '{ printf "%s ", $$5 }' | cat - $< | $(ICVM) $(ICBIN2OBJ) > $@ || ( cat $@ ; false )
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(TESTLOG)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)

.PHONY: skip
skip:
	@echo $(NAME): $(COLOR_RED)SKIPPED$(COLOR_NORMAL) >> $(TESTLOG)
	false

.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)

# Keep all automatically generated files (e.g. object files)
.SECONDARY:
