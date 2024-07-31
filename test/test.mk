ICDIR ?= $(abspath ../..)
include $(ICDIR)/intcode.mk

BINDIR ?= bin
OBJDIR ?= obj

# Ignore compilation errors, some tests expect compilation to fail
IC_ERROR_RESULT = true

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
	$(run-intcode-vm) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)

$(BINDIR)/%.txt: $(BINDIR)/%.input
	printf '$(NAME): executing ' >> $(TESTLOG)
	$(run-intcode-vm) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)

$(BINDIR)/%.input: $(OBJDIR)/%.o
	printf '$(NAME): linking ' >> $(TESTLOG)
	$(run-intcode-ld) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)
	printf '$(NAME): comparing map ' >> $(TESTLOG)
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@).map.yaml $@.map.yaml >> $(TESTLOG)

$(BINDIR)/%.a: $(OBJDIR)/%.o
	printf '$(NAME): archiving ' >> $(TESTLOG)
	$(run-intcode-ar) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)

$(OBJDIR)/%.o: %.s
	printf '$(NAME): assembling ' >> $(TESTLOG)
	$(run-intcode-as) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)

$(OBJDIR)/%.o: %.bin
	printf '$(NAME): running bin2obj ' >> $(TESTLOG)
	$(run-intcode-bin2obj) ; true
	TEST_DIFF_OPTIONAL=$(TEST_DIFF_OPTIONAL) ../diff-result.sh $(notdir $@) $@ >> $(TESTLOG)

.PHONY: skip
skip:
	@echo $(NAME): $(COLOR_RED)SKIPPED$(COLOR_NORMAL) >> $(TESTLOG)
	false

.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)

# Keep all automatically generated files (e.g. object files)
.SECONDARY:
