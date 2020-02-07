NAME = $(notdir $(CURDIR))
OUTDIR = output
LOGFILE = ../test.log

ICAS = $(abspath ../../vm)/ic $(abspath ../../bin/as.input)
ICLD = $(abspath ../../vm)/ic $(abspath ../../bin/ld.input)

HAVE_COLOR := $(or $(FORCE_COLOR), $(shell [ -n $$(tput colors) ] && [ $$(tput colors) -ge 8 ] && echo 1))
ifeq ($(HAVE_COLOR),1)
	COLOR_NORMAL := "$(shell tput sgr0)"
	COLOR_RED := "$(shell tput setaf 1)"
	COLOR_GREEN := "$(shell tput setaf 2)"
endif

default: test

prep:
	rm -rf $(OUTDIR)
	mkdir -p $(OUTDIR)

$(OUTDIR)/%.input: $(OUTDIR)/%.o
	echo -n 'Linking $(NAME): ' >> $(LOGFILE)
	echo .$$ | cat $^ - | $(ICLD) > $@ 2> /dev/null || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

$(OUTDIR)/%.a: $(OUTDIR)/%.o
	echo -n 'Archiving $(NAME): ' >> $(LOGFILE)
	echo .L | cat - $^ > $@ || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

$(OUTDIR)/%.o: %.s
	echo -n 'Assembling $(NAME): ' >> $(LOGFILE)
	$(ICAS) < $< > $@ 2> /dev/null || true
	@diff $(notdir $@) $@ > /dev/null 2> /dev/null || \
		( echo $(COLOR_RED)FAILED$(COLOR_NORMAL) ; diff $(notdir $@) $@ ) >> $(LOGFILE)
	@echo $(COLOR_GREEN)OK$(COLOR_NORMAL) >> $(LOGFILE)

skip:
	@echo Testing $(NAME): $(COLOR_RED)SKIPPED$(COLOR_NORMAL) >> $(LOGFILE)
	false

clean:
	rm -rf $(OUTDIR)

.PHONY: default prep skip clean
