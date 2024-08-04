include ../test.mk

# Don't complain about missing .o, .input and .input.map.yaml files
TEST_DIFF_OPTIONAL = 1

# Save the stderr while running the test, since we need to verify it
ICVM_STDERR=$(BINDIR)/$(NAME).stderr

# Skip unless running under c-ext IC VM
ifeq ($(ICVM_TYPE),c-ext)
	TARGETS = test-prep $(BINDIR)/$(NAME).stdout
endif

test: $(TARGETS)
$(BINDIR)/$(NAME).stdout: $(NAME).stdin
