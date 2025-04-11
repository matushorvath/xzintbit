ICDIR ?= $(abspath .)
include $(ICDIR)/intcode.mk

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

# Build all fast Intcode VMs
.PHONY: build-fast-vms
build-fast-vms:
	make -C vms build-fast

# Build all tools
.PHONY: build-tools
build-tools: build-get-key build-profile

.PHONY: build-get-key
build-get-key:
	make -C vms build-c-ext
	make -C tools/get_key

.PHONY: build-profile
build-profile:
	npm --prefix tools/profile install
	npm --prefix tools/profile test

# Build stage 1
.PHONY: build-stage1
build-stage1:
	ICAS=$(abspath bin/as.input) ICBIN2OBJ=$(abspath bin/bin2obj.input) \
	ICLD=$(abspath bin/ld.input) ICLDMAP=$(abspath bin/ldmap.input) \
	BINDIR=$(abspath stage1) OBJDIR=$(abspath stage1) \
	make -C src build

# Build stage 2
.PHONY: build-stage2
build-stage2:
	ICAS=$(abspath stage1/as.input) ICBIN2OBJ=$(abspath stage1/bin2obj.input) \
	ICLD=$(abspath stage1/ld.input) ICLDMAP=$(abspath stage1/ldmap.input) \
	BINDIR=$(abspath stage2) OBJDIR=$(abspath stage2) \
	make -C src build

# Compare and install
.PHONY: compare-stages
compare-stages:
	diff -r stage1/as.input stage2/as.input
	diff -r stage1/bin2obj.input stage2/bin2obj.input
	diff -r stage1/ld.input stage2/ld.input
	diff -r stage1/ldmap.input stage2/ldmap.input
	diff -r stage1/libxib.a stage2/libxib.a
	diff -r stage1/as.input.map.yaml stage2/as.input.map.yaml
	diff -r stage1/bin2obj.input.map.yaml stage2/bin2obj.input.map.yaml
	diff -r stage1/ld.input.map.yaml stage2/ld.input.map.yaml
	diff -r stage1/ldmap.input.map.yaml stage2/ldmap.input.map.yaml

.PHONY: install
install:
	cp stage2/as.input bin/as.input
	cp stage2/as.input.map.yaml bin/as.input.map.yaml
	cp stage2/bin2obj.input bin/bin2obj.input
	cp stage2/bin2obj.input.map.yaml bin/bin2obj.input.map.yaml
	cp stage2/ld.input bin/ld.input
	cp stage2/ld.input.map.yaml bin/ld.input.map.yaml
	cp stage2/ldmap.input bin/ldmap.input
	cp stage2/ldmap.input.map.yaml bin/ldmap.input.map.yaml
	cp stage2/libxib.a bin/libxib.a

# Test
.PHONY: test
test: build run-test

.PHONY: run-test
run-test:
	MAKE=$(MAKE) TESTLOG="$(TESTLOG)" TESTDIRS="$(TESTDIRS)" \
		./test/test-vms.sh $(ICVM_TYPE)

# Test with all VMs
.PHONY: test-vms
test-vms: build-vms build run-test-vms

# Test with all fast VMs
.PHONY: test-fast-vms
test-fast-vms: build-fast-vms build run-test-fast-vms

.PHONY: run-test-vms
run-test-vms:
	MAKE=$(MAKE) TESTLOG="$(TESTLOG)" TESTDIRS="$(TESTDIRS)" \
		./test/test-vms.sh apl c cl cs go js js-fn rust

.PHONY: run-test-fast-vms
run-test-fast-vms:
	MAKE=$(MAKE) TESTLOG="$(TESTLOG)" TESTDIRS="$(TESTDIRS)" \
		./test/test-vms.sh c cl cs go js rust

# Clean
.PHONY: clean
clean:
	for testdir in $(TESTDIRS) ; do $(MAKE) -C $$testdir clean ; done
	$(MAKE) -C vms clean
	$(MAKE) -C src clean
	rm -rf $(TESTLOG)
	rm -rf *.tmp
	rm -rf stage1 stage2
