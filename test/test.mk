# diff-result needs .ONESHELL
.ONESHELL:
.SHELLFLAGS += -e

ICVM_TYPE ?= c
ICVM ?= $(abspath ../../vms)/$(ICVM_TYPE)/ic

IC_BINDIR ?= $(abspath ../../bin)
ICAS ?= $(IC_BINDIR)/as.input
ICBIN2OBJ ?= $(IC_BINDIR)/bin2obj.input
ICLD ?= $(IC_BINDIR)/ld.input

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

define diff-result
	# if the template file exists, or diffs are mandatory, run diff
	if [ "x$(TEST_DIFF_OPTIONAL)" = "x" ] || [ -f "$(notdir $@)" ]
	then
		if ! diff $(notdir $@) $@ > /dev/null 2> /dev/null
		then
			echo $(COLOR_RED)FAILED$(COLOR_NORMAL) >> $(TESTLOG)
			diff $(notdir $@) $@ >> $(TESTLOG)
		fi
	fi

	echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(TESTLOG)
endef

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
	$(diff-result)

$(BINDIR)/%.txt: $(BINDIR)/%.input
	printf '$(NAME): executing ' >> $(TESTLOG)
	$(ICVM) $< > $@ || ( cat $@ ; true )
	$(diff-result)

$(BINDIR)/%.input: $(OBJDIR)/%.o
	printf '$(NAME): linking ' >> $(TESTLOG)
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; true )
	$(diff-result)

$(BINDIR)/%.a: $(OBJDIR)/%.o
	printf '$(NAME): archiving ' >> $(TESTLOG)
	cat $^ | sed 's/^.C$$/.L/g' > $@ || true
	$(diff-result)

$(OBJDIR)/%.o: %.s
	printf '$(NAME): assembling ' >> $(TESTLOG)
	cat $^ | $(ICVM) $(ICAS) > $@ || ( cat $@ ; true )
	$(diff-result)

$(OBJDIR)/%.o: %.bin
	printf '$(NAME): running bin2obj ' >> $(TESTLOG)
	wc -c $< | cat - $< | $(ICVM) $(ICBIN2OBJ) > $@ || ( cat $@ ; false )
	$(diff-result)

.PHONY: skip
skip:
	@echo $(NAME): $(COLOR_RED)SKIPPED$(COLOR_NORMAL) >> $(TESTLOG)
	false

.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)

# Keep all automatically generated files (e.g. object files)
.SECONDARY:
