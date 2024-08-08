ICDIR ?= $(abspath ../..)
ICVM_TYPE = c-ext
include $(ICDIR)/intcode.mk

BINDIR ?= bin
OBJDIR ?= obj

.PHONY: build
build: build-prep $(BINDIR)/get_key.input

.PHONY: build-prep
build-prep:
	mkdir -p "$(BINDIR)" "$(OBJDIR)"

.PHONY: run
run: build
	$(ICVM) $(BINDIR)/get_key.input

$(BINDIR)/get_key.input: $(OBJDIR)/get_key.o $(LIBXIB)
	$(run-intcode-ld)

$(OBJDIR)/get_key.o: get_key.s
	$(run-intcode-as)

.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)
