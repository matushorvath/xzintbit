ICVM ?= $(abspath ../vm)/ic
ICAS ?= $(abspath ../bin/as.input)
ICLD ?= $(abspath ../bin/ld.input)

BINDIR ?= bin
OBJDIR ?= obj

# Build
.PHONY: build
build: build-prep $(BINDIR)/as.input $(BINDIR)/ld.input

.PHONY: build-prep
build-prep:
	mkdir -p $(BINDIR) $(OBJDIR)

$(BINDIR)/as.input: $(OBJDIR)/as.o

$(BINDIR)/ld.input: $(OBJDIR)/ld.o

$(BINDIR)/%.input: $(OBJDIR)/%.o
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; false )

$(OBJDIR)/%.o: %.s
	$(ICVM) $(ICAS) < $< > $@ || ( cat $@ ; false )

# Clean
.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)