CFLAGS = -O3 -Wall -Werror -std=c11

DOTNET_RID = $(shell dotnet --info | awk '/RID:/ { print $$2 }')

export GOFLAGS

.PHONY: build
build: build-apl build-c build-c-ext build-cs build-cl build-go build-js build-js-fn build-rust

.PHONY: build-fast
build-fast: build-c build-c-ext build-cs build-cl build-go build-js build-rust

.PHONY: clean
clean: clean-apl clean-c clean-c-ext clean-cs clean-cl clean-go clean-js clean-js-fn clean-rust

# Intcode VM in C
.PHONY: build-c
build-c: c/ic

.PHONY: clean-c
clean-c:
	rm -rf c/ic c/ic.exe c/*.o

# Extended intcode VM in C
.PHONY: build-c-ext
build-c-ext: c-ext/ic

c-ext/ic: c-ext/ic.o c-ext/profile.o c-ext/signals.o c-ext/terminal.o
c-ext/ic.o: c-ext/profile.h c-ext/signals.h c-ext/terminal.h
c-ext/profile.o: c-ext/profile.h
c-ext/signals.o: c-ext/signals.h c-ext/profile.h c-ext/terminal.h
c-ext/terminal.o: c-ext/terminal.h

.PHONY: clean-c-ext
clean-c-ext:
	rm -rf c-ext/ic c-ext/ic.exe c-ext/*.o

# Intcode VM in Go
.PHONY: build-go
build-go: go/ic
go/ic: go/ic.go
	( cd go; go build -o ic )

.PHONY: clean-go
clean-go:
	rm -rf go/ic go/ic.exe

# Intcode VM in C#
.PHONY: build-cs
build-cs: cs/ic
cs/ic: cs/ic.cs cs/ic.csproj
	dotnet publish cs -c Release -r $(DOTNET_RID) -p:PublishSingleFile=true --self-contained false
	if [ "$$OS" = "Windows_NT" ] ; then cp cs/bin/Release/net6.0/$(DOTNET_RID)/publish/ic.exe cs/ ; fi
	if [ "$$OS" != "Windows_NT" ] ; then cp cs/bin/Release/net6.0/$(DOTNET_RID)/publish/ic cs/ ; fi

.PHONY: clean-cs
clean-cs:
	rm -rf cs/ic cs/ic.cmd cs/bin cs/obj

# Intcode VM in Rust
.PHONY: build-rust
build-rust: rust/ic
rust/ic: rust/Cargo.toml rust/*.rs
	( cd rust ; cargo build --release )
	if [ "$$OS" = "Windows_NT" ] ; then cp rust/target/release/ic.exe rust/ ; fi
	if [ "$$OS" != "Windows_NT" ] ; then cp rust/target/release/ic rust/ ; fi

.PHONY: clean-rust
clean-rust:
	rm -rf rust/ic rust/ic.exe rust/target

# Intcode VM in Common Lisp
.PHONY: build-cl
build-cl: cl/ic
cl/ic: cl/ic.lisp
	ln -sf ic.lisp cl/ic

.PHONY: clean-cl
clean-cl:
	rm -rf cl/ic

# Intcode VM in Javascript
.PHONY: build-js
build-js:
	( cd js ; npm install ; npm test )

.PHONY: clean-js
clean-js:
	rm -rf js/node_modules

# Intcode VM in Javascript, implemented as a single function
.PHONY: build-js-fn
build-js-fn: js-fn/ic js-fn/ic.mjs

.PHONY: clean-js-fn
clean-js-fn:

# Intcode VM in APL
.PHONY: build-apl
build-apl: apl/ic apl/ic.apl

.PHONY: clean-apl
clean-apl:
