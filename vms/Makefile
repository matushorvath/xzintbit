CFLAGS = -O3 -Wall -Werror -std=c11

DOTNET_RID = $(shell dotnet --info | awk '/RID:/ { print $$2 }')

ifeq ($(ICVM_PROFILE),y)
	CFLAGS += -DICVM_PROFILE
	GOFLAGS += --tags=profile
endif

export GOFLAGS

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
	go build -o $@ ./go

.PHONY: clean-go
clean-go:
	rm -rf go/ic go/ic.exe

# Intcode VM in C#
.PHONY: build-cs
build-cs: cs/ic
cs/ic: cs/ic.cs cs/ic.csproj
	dotnet publish cs -c Release -r $(DOTNET_RID) -p:PublishSingleFile=true --self-contained false
	if [ "$$OS" = "Windows_NT" ] ; then cp cs/bin/Release/net5.0/$(DOTNET_RID)/publish/ic.exe cs/ ; fi
	if [ "$$OS" != "Windows_NT" ] ; then cp cs/bin/Release/net5.0/$(DOTNET_RID)/publish/ic cs/ ; fi

.PHONY: clean-cs
clean-cs:
	rm -rf cs/ic cs/ic.cmd cs/bin cs/obj
