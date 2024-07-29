ICDIR ?= $(abspath ..)
include $(ICDIR)/intcode.mk

SRCDIR = .
BINDIR ?= bin
OBJDIR ?= obj

# Build
.PHONY: build
build: build-prep $(BINDIR)/as.input $(BINDIR)/asd.input $(BINDIR)/bin2obj.input \
	$(BINDIR)/ld.input $(BINDIR)/ldmap.input

.PHONY: build-prep
build-prep:
	mkdir -p $(BINDIR) $(OBJDIR)/as $(OBJDIR)/ld $(OBJDIR)/libxib $(OBJDIR)/tools

AS_OBJS = child.o directive.o error.o fixup.o frame.o global.o keyword.o lexer.o memory.o \
	object.o parse_call_ret.o parse_data.o parse_eof_eoi.o parse_frame.o parse_instruction.o \
	parse_param.o parse_symbol.o parser.o

$(BINDIR)/as.input: $(addprefix $(OBJDIR)/as/, as.o $(AS_OBJS)) $(BINDIR)/libxib.a
	$(run-intcode-ld)

$(BINDIR)/asd.input: $(addprefix $(OBJDIR)/as/, asd.o $(AS_OBJS)) $(BINDIR)/libxib.a
	$(run-intcode-ld)

$(OBJDIR)/as/%.o: $(SRCDIR)/as/%.s $(SRCDIR)/as/const.si
	$(run-intcode-as)

BIN2OBJ_OBJS = bin2obj.o

$(BINDIR)/bin2obj.input: $(addprefix $(OBJDIR)/tools/, $(BIN2OBJ_OBJS)) $(BINDIR)/libxib.a
	$(run-intcode-ld)

$(OBJDIR)/tools/%.o: $(SRCDIR)/tools/%.s
	$(run-intcode-as)

LD_OBJS = ld.o process.o data.o error.o link.o load.o output.o relocate.o resolve.o

$(BINDIR)/ld.input: $(addprefix $(OBJDIR)/ld/, $(LD_OBJS)) $(BINDIR)/libxib.a
	$(run-intcode-ld)

$(OBJDIR)/ld/%.o: $(SRCDIR)/ld/%.s $(SRCDIR)/ld/const.si
	$(run-intcode-as)

LDMAP_OBJS = ldmap.o process.o data.o error.o link.o load.o output.o relocate.o resolve.o

$(BINDIR)/ldmap.input: $(addprefix $(OBJDIR)/ld/, $(LDMAP_OBJS)) $(BINDIR)/libxib.a
	$(run-intcode-ld)

LIBXIB_OBJS = libxib.o brk.o error.o heap.o input.o lexer.o memory.o print.o string.o

$(BINDIR)/libxib.a: $(addprefix $(OBJDIR)/libxib/, $(LIBXIB_OBJS))
	$(run-intcode-ar)

$(OBJDIR)/libxib/%.o: $(SRCDIR)/libxib/%.s
	$(run-intcode-as)

# Clean
.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)
