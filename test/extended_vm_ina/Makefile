include ../test.mk

# Don't complain about missing .o, .input and .input.map.yaml files
TEST_DIFF_OPTIONAL = 1

# Skip unless running under c-ext IC VM
ifeq ($(ICVM_TYPE),c-ext)
	TARGETS = test-prep $(BINDIR)/$(NAME).stdout
endif

test: $(TARGETS)
$(BINDIR)/$(NAME).stdout: $(NAME).stdin
