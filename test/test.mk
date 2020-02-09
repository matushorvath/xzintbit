ICVM ?= $(abspath ../../vm)/ic
ICAS ?= $(abspath ../../bin/as.input)
ICLD ?= $(abspath ../../bin/ld.input)

BINDIR ?= bin
OBJDIR ?= obj

NAME = $(notdir $(CURDIR))
LOGFILE = ../test.log

HAVE_COLOR := $(or $(FORCE_COLOR), $(shell [ -n $$(tput colors) ] && [ $$(tput colors) -ge 8 ] && echo 1))
ifeq ($(HAVE_COLOR),1)
	COLOR_NORMAL := "$(shell tput sgr0)"
	COLOR_RED := "$(shell tput setaf 1)"
	COLOR_GREEN := "$(shell tput setaf 2)"
endif

.PHONY: default test
default: test

.PHONY: test-prep
test-prep:
	rm -rf $(BINDIR) $(OBJDIR)
	mkdir -p $(BINDIR) $(OBJDIR)

$(BINDIR)/%.input: $(OBJDIR)/%.o
	echo -n '$(NAME): linking ' >> $(LOGFILE)
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ 2> /dev/null || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

$(BINDIR)/%.a: $(OBJDIR)/%.o
	echo -n '$(NAME): archiving ' >> $(LOGFILE)
	echo .L | cat - $^ > $@ || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

$(OBJDIR)/%.o: %.s
	echo -n '$(NAME): assembling ' >> $(LOGFILE)
	$(ICVM) $(ICAS) < $< > $@ 2> /dev/null || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

.PHONY: skip
skip:
	@echo $(NAME): $(COLOR_RED)SKIPPED$(COLOR_NORMAL) >> $(LOGFILE)
	false

.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)

# Keep all automatically generated files (e.g. object files)
.SECONDARY:
