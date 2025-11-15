[![Build and Test](https://github.com/matushorvath/xzintbit/actions/workflows/build.yml/badge.svg)](https://github.com/matushorvath/xzintbit/actions/workflows/build.yml)

What is this?
-------------

Glad you asked! This is a [self-hosted](https://en.wikipedia.org/wiki/Self-hosting_(compilers)) [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) and [linker](https://en.wikipedia.org/wiki/Linker_(computing)) for [Intcode](https://adventofcode.com/2019/day/9). In other words, this is a compiler package that helps you write Intcode programs in a human-readable assembly language, and the compiler itself is written in the same human-readable assembly language.

How can I try it out?
---------------------

Let's write a sample *"Hello, World!"* program:

```asm
    arb message

loop:
    jz  [rb], done
    out [rb]
    arb 1
    jz  0, loop

done:
    out 10
    hlt

message:
    db  "Hello, world!", 0

.EOF
```

Save this program into a file called [`hello-world.s`](test/hello-world.s).
To run it, you will need to assemble and link it, creating an Intcode executable.

The assembler and linker themselves are also written in Intcode, so you will need an Intcode virtual machine (VM) to run them. You can definitely [use your own Intcode VM](#will-it-run-on-my-virtual-machine), or you could use mine.

My Intcode VM is written in C, so to build it you will need a (relatively modern) C compiler. To build it, run `make build-vm`. It is started by executing `vms/c/ic` (or `vms\c\ic.exe` on Windows), reads from standard input and writes to standard output.

```sh
$ make build-vm
$ vms/c/ic bin/as.input < hello-world.s > hello-world.o
```

Here `bin/as.input` is the assembler program from this repo, `hello-world.s` is our example from above, and `hello-world.o` is an object file. The object file now needs to be linked to create an Intcode binary.

```sh
$ echo .$ | cat hello-world.o - | vms/c/ic bin/ld.input > hello-world.input
```

Here `bin/ld.input` is the linker program from this repo, `hello-world.o` is the object file created by the assembler, and `hello-world.input` is the compiled *"Hello, World!"* program in Intcode. Yes, the output file name ends in `.input`. That's just the extension I decided to use for Intcode binaries.

The command line for the linker is more complex than the assembler one. Assembler always takes one input, but the linker can be used to link multiple object files into one binary. Since Intcode has no file system access, we need to pass all object files via standard output. And since Intcode has no way of detecting end of input, we need to follow that up with a `.$` statement to mark the end of last object file.

If everything went well, `hello-world.input` should now contain this compiled Intcode program:

```json
109,15,1206,0,12,204,0,109,1,1106,0,2,104,10,99,72,101,108,108,111,44,32,119,111,114,108,100,33,0
```

You can see that the first instruction (`arb message`) is `109, 15` which adjusts the relative base by 15, the address of `message` in memory.

The next instruction (`jz [rb], done`) is `1206,0,12`, which is a jump-if-false with one relative and one immediate parameter, jumping to address 12 which is the `done` label. And so on. The program loops over the characters of `message` while increasing the relative base (`rb`) by one, until it finds a zero character, then outputs a new line (`10`) and halts.

Now we can run the program. Of course, we will need our Intcode VM again:

```sh
$ vms/c/ic hello-world.input
Hello, world!
```

Hey, it works!

Will it run on my (virtual) machine?
------------------------------------

You should be able to execute both the assembler and linker on your own Intcode virtual machine, as long as it follows some basic principles:

- The VM needs to be a complete Intcode computer, as specified in [Day 9](https://adventofcode.com/2019/day/9).
- It also needs the *Aft Scaffolding Control and Information Interface* (ASCII) from [Day 17](https://adventofcode.com/2019/day/17). Which just means that everything output by Intcode should be displayed as ASCII characters, with character `10` meaning 'new line'.
- The VM needs to have enough memory.
   - Ideally memory should expand automatically as needed. Expanding it by reallocating a larger array will work, since memory is accessed in one contiguous memory block starting at address 0.
   - A statically sized memory implementation could also work, as long as it the memory is large enough. For example, compiling the assembler itself takes around 100kB of Intcode memory.
- The VM must not output any extra messages, other than what is output by Intcode using `out` instructions. If your VM outputs any extra messages of its own, they will mix up with the compiled Intcode output and the result will not be valid Intcode, obviously.
- There is no need for arbitrary sized numbers, even implementations based on 32-bit integers should be fine.
- Ideally your VM should exit with a non-zero exit code (or just crash) when an `in` instruction does not have any more data to input. This is used to signal compilation errors, since it's the only way an Intcode program can return a non-zero exit code.

Self-hosted?
------------

Yes, both the assembler and linker are written in their own language, so they compile themselves. There is of course a chicken-and-egg problem hidden here, you need a compiled assembler and linker to compile the assembler and linker.

That's why in addition to the assembler source code in `src` directory, this git repo also contains Intcode binaries in `bin`.

How can you [trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf) me that my binaries were compiled from this source? You could try compiling them yourself and comparing your binaries with mine. There is a helpful makefile in the repo that does that for you:

```sh
$ make && make install && make test
```

This compiles sources in `src` using binaries in `bin` into new binaries in `stage1` directory, then uses the new binaries to compile the same sources again into another set of binaries in `stage2` directory. If everything works, the two generated binaries should be the same and they are copied to `bin`. Then it runs unit tests on the binaries in `bin` directory.

What can it do?
-----------------

In addition to supporting all Intcode features specified in [Day 9](https://adventofcode.com/2019/day/9), the assembler also supports:

- Symbols that can be used instead of numeric addresses.
- Stack based on relative addressing mode.
- Pseudo-instructions call and ret to simplify using functions.
- Directives .FRAME and .ENDFRAME to simplify using local variables.
- Pseudo-instructions db and ds to define memory contents directly.

The stack and function call support is really what made this project managable. It allows you to use something close to structured programming, based on functions that have well-defined reusable interfaces, with arguments and return values.

More detailed description of the language can be found in [doc/Language.md](doc/Language.md). Language grammar is available in [doc/Grammar.txt](doc/Grammar.txt).

The linker supports arbitary number of inputs, performing relocations on input object files to simulate position-independent code, and fixing up exported and imported symbols to allow objects to reference each other. It also supports static libraries, selecting only referenced objects from each library.

But why?
--------
I heard you liked assembler, so I assembled an assembler in assembler, so you can assemble while you assemble.

![Xzibit](https://i.kym-cdn.com/photos/images/small/000/001/122/xzibit-happy.jpg)
