include ../test.mk

test: test-prep $(BINDIR)/$(NAME).input
$(BINDIR)/$(NAME).input: $(OBJDIR)/$(NAME).o $(BINDIR)/$(NAME)_lib.a
$(BINDIR)/$(NAME)_lib.a: $(OBJDIR)/$(NAME)_lib.o $(OBJDIR)/$(NAME)_lib_2.o
