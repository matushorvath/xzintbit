CFLAGS = -O3 -Wall -Werror -std=c11

ifeq ($(ICVM_PROFILE),y)
	CFLAGS += -DICVM_PROFILE
endif

.PHONY: build
build: build-vm-c

.PHONY: clean
clean: clean-c

# Intcode VM in C
.PHONY: build-c
build-c: c/ic
c/ic: c/ic.o c/profile.o

.PHONY: clean-c
clean-c:
	rm -rf c/ic c/ic.exe c/*.o
