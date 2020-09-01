include ../test.mk

test: test-prep $(OBJDIR)/$(NAME).o

# $(NAME).s is a dummy file, since make insists on having it first (before any includes)
$(OBJDIR)/$(NAME).o: $(NAME).s include.si main.s
