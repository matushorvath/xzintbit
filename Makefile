TESTDIRS = $(sort $(dir $(wildcard test/*/*)))
CFLAGS = -O3 -Wall -Werror -std=c11

.PHONY: build
build: build-vm build-stage1 build-stage2 compare-stages install

# Build Intcode VM
.PHONY: build-vm
build-vm: vm/ic
vm/ic: vm/ic.o

# Build stage 1
.PHONY: build-stage1
build-stage1:
	ICAS=$(abspath bin/as.input) \
	ICLD=$(abspath bin/ld.input) \
	BINDIR=$(abspath stage1) \
	OBJDIR=$(abspath stage1) \
	make -C src build

# Build stage 2
.PHONY: build-stage2
build-stage2:
	ICAS=$(abspath stage1/as.input) \
	ICLD=$(abspath stage1/ld.input) \
	BINDIR=$(abspath stage2) \
	OBJDIR=$(abspath stage2) \
	make -C src build

# Compare and install
.PHONY: compare-stages
compare-stages:
	diff -r stage1/as.input stage2/as.input
	diff -r stage1/ld.input stage2/ld.input

.PHONY: install
install:
	cp stage2/as.input bin/as.input
	cp stage2/ld.input bin/ld.input

# Test
.PHONY: install
test: build
	rm -rf test/test.log
	failed=0 ; \
	for testdir in $(TESTDIRS) ; do \
		$(MAKE) -C $$testdir test || failed=1 ; \
	done ; \
	cat test/test.log ; \
	[ $$failed = 0 ] || exit 1

# Clean
.PHONY: clean
clean:
	for testdir in $(TESTDIRS) ; do $(MAKE) -C $$testdir clean ; done
	rm -rf test/test.log
	rm -rf *.tmp
	rm -rf stage1 stage2
	rm -rf vm/ic vm/ic.exe vm/ic.o
