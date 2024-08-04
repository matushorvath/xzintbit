include ../test.mk

# Don't complain about missing .o, .input and .input.map.yaml files, we don't want to compare those
TEST_DIFF_OPTIONAL = 1

test: test-prep $(BINDIR)/$(NAME).stdout
$(BINDIR)/$(NAME).input: $(OBJDIR)/$(NAME).o $(LIBXIB)
