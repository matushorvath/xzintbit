include ../test.mk

# Don't complain about missing .o and .input files, we don't want to compare those
TEST_DIFF_OPTIONAL = 1

test: test-prep $(BINDIR)/$(NAME).txt
$(BINDIR)/$(NAME).input: $(OBJDIR)/$(NAME).o $(IC_BINDIR)/libxib.a
