include ../test.mk

test: test-prep $(BINDIR)/$(NAME).input

$(BINDIR)/$(NAME).input: $(OBJDIR)/$(NAME).o $(OBJDIR)/$(NAME)_2.o

$(OBJDIR)/$(NAME).o: $(NAME).o
	# This kind of .o file can't be produced by as, but we want to test how ld handles it
	cp $< $@
