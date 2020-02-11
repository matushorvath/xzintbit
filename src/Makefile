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

$(BINDIR)/as.input: $(OBJDIR)/as.o $(OBJDIR)/lexer.o $(OBJDIR)/util.o $(BINDIR)/libxib.a

$(BINDIR)/ld.input: $(OBJDIR)/ld.o $(BINDIR)/libxib.a

$(BINDIR)/libxib.a: $(OBJDIR)/libxib.o $(OBJDIR)/error.o $(OBJDIR)/heap.o $(OBJDIR)/print.o $(OBJDIR)/string.o

$(BINDIR)/%.input: $(OBJDIR)/%.o
	echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; false )

$(BINDIR)/%.a: $(OBJDIR)/%.o
	echo .L | cat - $^ > $@ || ( cat $@ ; false )

$(OBJDIR)/%.o: */%.s
	$(ICVM) $(ICAS) < $< > $@ || ( cat $@ ; false )

# Clean
.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)
