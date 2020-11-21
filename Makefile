ICVM_TYPE ?= c
export ICVM_TYPE

TESTDIRS = $(sort $(dir $(wildcard test/*/*)))
export TESTLOG = $(abspath test/test.log)

.PHONY: build
build: build-vm build-stage1 build-stage2 compare-stages install

# Build the default Intcode VM
.PHONY: build-vm
build-vm:
	make -C vms build-$(ICVM_TYPE)

# Build all Intcode VMs
.PHONY: build-vms
build-vms:
	make -C vms build

# Build stage 1
.PHONY: build-stage1
build-stage1:
	ICAS=$(abspath bin/as.input) ICLD=$(abspath bin/ld.input) \
	BINDIR=$(abspath stage1) OBJDIR=$(abspath stage1) \
	make -C src build

# Build stage 2
.PHONY: build-stage2
build-stage2:
	ICAS=$(abspath stage1/as.input) ICLD=$(abspath stage1/ld.input) \
	BINDIR=$(abspath stage2) OBJDIR=$(abspath stage2) \
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
.PHONY: test
test: build
	rm -rf $(TESTLOG)
	failed=0 ; \
	for testdir in $(TESTDIRS) ; do \
		$(MAKE) -C $$testdir test || failed=1 ; \
	done ; \
	cat test/test.log ; \
	[ $$failed = 0 ] || exit 1

# Test with all VMs
.PHONY: test-vms
test-vms: build-vms build
	rm -rf $(TESTLOG)
	failed=0 ; \
	for type in c go ; do \
		echo "====================" >> $(TESTLOG) ; \
		echo "ICVM_TYPE = $$type" >> $(TESTLOG) ; \
		for testdir in $(TESTDIRS) ; do \
			ICVM_TYPE=$$type $(MAKE) -C $$testdir test || failed=1 ; \
		done ; \
	done ; \
	cat test/test.log ; \
	[ $$failed = 0 ] || exit 1

# Clean
.PHONY: clean
clean:
	for testdir in $(TESTDIRS) ; do $(MAKE) -C $$testdir clean ; done
	$(MAKE) -C vms clean
	$(MAKE) -C src clean
	rm -rf $(TESTLOG)
	rm -rf *.tmp
	rm -rf stage1 stage2
