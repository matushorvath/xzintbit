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
build: build-prep $(BINDIR)/as.input $(BINDIR)/ld.input print-profile

.PHONY: build-prep
build-prep:
	mkdir -p $(BINDIR) $(OBJDIR)/as $(OBJDIR)/ld $(OBJDIR)/libxib
ifeq ($(ICVM_PROFILE),y)
	rm -f icvm_profile.csv
endif

AS_OBJS = as.o directive.o error.o fixup.o frame.o global.o keyword.o lexer.o memory.o object.o \
	parse_call_ret.o parse_data.o parse_eof_eoi.o parse_frame.o parse_instruction.o parse_param.o \
	parse_symbol.o parser.o

$(BINDIR)/as.input: $(addprefix $(OBJDIR)/as/, $(AS_OBJS)) $(BINDIR)/libxib.a
	$(run-ld)

$(OBJDIR)/as/%.o: $(SRCDIR)/as/const.si $(SRCDIR)/as/%.s
	$(run-as)

LD_OBJS = ld.o data.o error.o link.o load.o output.o relocate.o resolve.o

$(BINDIR)/ld.input: $(addprefix $(OBJDIR)/ld/, $(LD_OBJS)) $(BINDIR)/libxib.a
	$(run-ld)

$(OBJDIR)/ld/%.o: $(SRCDIR)/ld/const.si $(SRCDIR)/ld/%.s
	$(run-as)

LIBXIB_OBJS = libxib.o error.o heap.o input.o lexer.o memory.o print.o string.o

$(BINDIR)/libxib.a: $(addprefix $(OBJDIR)/libxib/, $(LIBXIB_OBJS))
	$(run-ar)

$(OBJDIR)/libxib/%.o: $(SRCDIR)/libxib/const.si $(SRCDIR)/libxib/%.s
	$(run-as)

define PROFILE_AWK
{
	max_mem_size = $$1 > max_mem_size ? $$1 : max_mem_size;
	inst_count += $$2;
	min_value = $$3 < min_value ? $$3 : min_value;
	max_value = $$4 > max_value ? $$4 : max_value;
}
END {
	printf("summary: max_mem_size = %d inst_count = %d min_value = %d max_value = %d\n",
		max_mem_size, inst_count, min_value, max_value);
}
endef
export PROFILE_AWK

.PHONY: print-profile
print-profile:
ifeq ($(ICVM_PROFILE),y)
	[ ! -f icvm_profile.csv ] || < icvm_profile.csv awk -F, "$$PROFILE_AWK"
endif

# Clean
.PHONY: clean
clean:
	rm -rf $(BINDIR) $(OBJDIR)
