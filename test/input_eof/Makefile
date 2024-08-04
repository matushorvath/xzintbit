include ../test.mk

# Save the stderr while running the test, since we need to verify it
ICVM_STDERR=$(BINDIR)/$(NAME).stderr

test: test-prep $(BINDIR)/$(NAME).stdout
$(BINDIR)/$(NAME).stdout: $(NAME).stdin
