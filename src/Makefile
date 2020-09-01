ICVM ?= $(abspath ../vm)/ic
ICAS ?= $(abspath ../bin/as.input)
ICLD ?= $(abspath ../bin/ld.input)

SRCDIR = .
BINDIR ?= bin
OBJDIR ?= obj

define run-as
cat $^ | $(ICVM) $(ICAS) > $@ || ( cat $@ ; false )
endef

define run-ar
echo .L | cat - $^ > $@ || ( cat $@ ; false )
endef

define run-ld
echo .$$ | cat $^ - | $(ICVM) $(ICLD) > $@ || ( cat $@ ; false )
endef

# Build
.PHONY: build
build: build-prep $(BINDIR)/as.input $(BINDIR)/ld.input

.PHONY: build-prep
build-prep:
	mkdir -p $(BINDIR) $(OBJDIR)/as $(OBJDIR)/ld $(OBJDIR)/libxib

AS_OBJS = as.o as_split.o lexer.o memory.o object.o util.o
$(BINDIR)/as.input: $(addprefix $(OBJDIR)/as/, $(AS_OBJS)) $(BINDIR)/libxib.a
	$(run-ld)

$(OBJDIR)/as/%.o: $(SRCDIR)/as/const.si $(SRCDIR)/as/%.s
	$(run-as)

LD_OBJS = ld.o ld_split.o memory.o
$(BINDIR)/ld.input: $(addprefix $(OBJDIR)/ld/, $(LD_OBJS)) $(BINDIR)/libxib.a
	$(run-ld)

$(OBJDIR)/ld/%.o: $(SRCDIR)/ld/const.si $(SRCDIR)/ld/%.s
	$(run-as)

LIBXIB_OBJS = libxib.o error.o heap.o print.o string.o
$(BINDIR)/libxib.a: $(addprefix $(OBJDIR)/libxib/, $(LIBXIB_OBJS))
	$(run-ar)

$(OBJDIR)/libxib/%.o: $(SRCDIR)/libxib/%.s
	$(run-as)

# Clean
.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)
