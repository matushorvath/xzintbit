CFLAGS = -O3 -Wall -Werror -std=c11

ifeq ($(ICVM_PROFILE),y)
	CFLAGS += -DICVM_PROFILE
endif

.PHONY: build
build: build-c build-go build-cs

.PHONY: clean
clean: clean-c clean-go clean-cs

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

# Intcode VM in C#
.PHONY: build-cs
build-cs: cs/ic
cs/ic: cs/ic.cs cs/ic.csproj
	dotnet build cs -c Release
	if [ "$$OS" = "Windows_NT] ; then \
		mklink cs/ic.exe cs/bin/Release/net5.0/ic.exe ; \
	else \
		ln -s bin/Release/net5.0/ic cs/ic ; \
	fi
	ls -l cs
	ls -l cs/bin
	ls -l cs/bin/Release
	ls -l cs/bin/Release/net5.0

.PHONY: clean-cs
clean-cs:
	rm -rf cs/ic cs/bin cs/obj
