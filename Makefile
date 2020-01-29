ifeq ($(OS), Windows_NT)
	RMRF = RMDIR /S /Q $1
	MKDIR = $(if $(wildcard $1),,MKDIR $1)
else
	MKDIR = mkdir -p $1
	RMRF = rm -rf $1
endif

TESTDIRS = $(sort $(dir $(wildcard test/*/)))
CFLAGS = -O3 -Wall -Werror -std=c11

build: build-vm build-stage1 build-stage2 compare-stages

# Build Intcode VM
build-vm: vm/ic
vm/ic: vm/ic.o

# Build stage 1
build-stage1: stage1 stage1/as.input stage1/ld.input

stage1:
	$(MKDIR) stage1

stage1/as.input: stage1/as.o

stage1/ld.input: stage1/ld.o

stage1/%.input: stage1/%.o
	echo .$$ | cat $^ - | vm/ic bin/ld.input > $@

stage1/%.o: src/%.s
	vm/ic bin/as.input < $< > $@

# Build stage 1
build-stage2: build-stage1 stage2 stage2/as.input stage2/ld.input

stage2:
	$(MKDIR) stage2

stage2/as.input: stage2/as.o

stage2/ld.input: stage2/ld.o

stage2/%.input: stage2/%.o
	echo .$$ | cat $^ - | vm/ic stage1/ld.input > $@

stage2/%.o: src/%.s
	vm/ic stage1/as.input < $< > $@

# Compare and install
compare-stages:
	diff -r stage1/as.input stage2/as.input
	diff -r stage1/ld.input stage2/ld.input

install: build
	cp stage2/as.input bin/as.input
	cp stage2/ld.input bin/ld.input

# Test
test: install
	for testdir in $(TESTDIRS) ; do $(MAKE) -C $$testdir test ; done

# Clean
clean:
	for testdir in $(TESTDIRS) ; do $(MAKE) -C $$testdir clean ; done
	$(RMRF) stage1 stage2
	$(RMRF) vm/ic vm/ic.exe vm/ic.o

.PHONY: build build-vm build-stage1 build-stage2 compare-stages test install clean
