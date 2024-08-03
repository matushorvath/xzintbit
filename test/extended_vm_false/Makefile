include ../test.mk

# Don't complain about missing .o, .input and .input.map.yaml files
TEST_DIFF_OPTIONAL = 1

# If running under c-ext IC VM, force the VM to run in standard mode
ifeq ($(ICVM_TYPE),c-ext)
	ICVM += -s
endif

test: test-prep $(BINDIR)/$(NAME).txt
