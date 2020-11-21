CFLAGS = -O3 -Wall -Werror -std=c11

ifeq ($(ICVM_PROFILE),y)
	CFLAGS += -DICVM_PROFILE
endif

.PHONY: build
build: build-c

.PHONY: clean
clean: clean-c

# Intcode VM in C
.PHONY: build-c
build-c: c/ic
c/ic: c/ic.o c/profile.o

.PHONY: clean-c
clean-c:
	rm -rf c/ic c/ic.exe c/*.o

# Intcode VM in Go
.PHONY: build-go
build-go: go/ic
go/ic: go/ic.go
	go build -o $@ $^

.PHONY: clean-go
clean-go:
	rm -rf go/ic go/ic.exe
