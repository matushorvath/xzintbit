What is this?
-------------

Glad you asked! This is a [self-hosted](https://en.wikipedia.org/wiki/Self-hosting_(compilers)) [assembler](https://en.wikipedia.org/wiki/Assembly_language#Assembler) for [Intcode](https://adventofcode.com/2019/day/9). In other words, this is a compiler that helps you write Intcode programs in a human-readable assembly language, and the compile itself is written in the same human-readable assembly language.

How can I try it out?
---------------------

Let's write a sample *"Hello, World!"* program:

```asm
    arb message

loop:
    jz  [rb + 0], done
    out [rb + 0]
    arb 1
    jz  0, loop

done:
    out 10
    hlt

message:
    db "Hello, world!", 0

.EOF
```

Save this program into a file called `hello-world.s`.
To run it, we will need to translate it to Intcode.

Since the assembler itself is also written in Intcode, we will need an Intcode virtual machine (VM) to run it. For now, let's assume your Intcode VM can be run by executing `vm.sh`, that it reads from standard input and writes to standard output.

```sh
$ ./vm.sh src/as.input < hello-world.s > hello-world.input
```

Here `src/as.input` is the assembler program, `hello-world.s` is our example from above, and `hello-world.input` is the compiled *"Hello, World!"* program in Intcode.

If everything went well, `hello-world.input` should now contain the following Intcode program:

```json
109,13,1206,0,12,204,0,109,1,1106,0,2,99,72,101,108,108,111,44,32,119,111,114,108,100,33,0
```

You can see that the first instruction (`arb message`) is `109, 13` which adjusts the relative base by 13, the address of `message` in memory.

The next instruction (`jz [rb + 0], done`) is `1206,0,12`, which is a jump-if-false with one relative and one immediate parameter, jumping to address 12 which is the `done` label. And so on.

Now we can run the program. Of course, we will need our Intcode VM again:

```sh
$ ./vm.sh hello-world.input
Hello, world!
```

Hey, it worked!

Will it run on my (virtual) machine?
--------------------------

You should be able to run the assembler with your own Intcode virtual machine without problems, as long as it follows some basic principles:

- It needs to be a complete Intcode computer, as specified in [Day 9](https://adventofcode.com/2019/day/9).
- It needs automatically expanding memory. The program allocates memory in one continuous block starting at address 0, so a VM implementation based on reallocating arrays is good enough.
- It needs the *Aft Scaffolding Control and Information Interface* (ASCII) from [Day 17](https://adventofcode.com/2019/day/17). Which just means that everything output by Intcode should be handled as ASCII characters, with character `10` meaning 'new line'.
- It must not output any extra messages to stdout, other than what is issued by Intcode using `out` instructions. If your VM outputs any extra messages of its own, they will mix up with the Intcode output and the result will not be valid Intcode, obviously.
- No need for arbitrary sized numbers, even implementations based on 32-bit integers should be fine.

If you don't have an IntCode VM, you can try the one in `vm` directory, which is the one I use. It's written in TypeScript, and you will need node.js 10.x or newer to run it.

```sh
$ cd vm
$ npm i
$ npx ts-node src/ic.ts ../src/as.input < ../doc/hello-world.s > hello-world.input
$ npx ts-node src/ic.ts hello-world.input
Hello, world!
```

Self-hosted?
------------

Yes, the assembler is written in its own language, so it compiles itself. There is of course a chicken-and-egg problem hidden here, you need the compiled assembler program to compile the assembler program.

That's why in addition to the source code in `src/as.s`, the git repo also contains the Intcode binary in `src/as.input`.

How can you [trust](https://www.cs.cmu.edu/~rdriley/487/papers/Thompson_1984_ReflectionsonTrustingTrust.pdf) me that my binary was compiled from this source? You could try compiling it yourself and comparing your binary with mine. There is a helpful shell script in the repo that does that for you:

```sh
$ ./make.sh
+ ./vm.sh src/as.input
+ status=0
+ '[' 0 -ne 0 ']'
+ ./vm.sh src/as.stg1.input
+ status=0
+ '[' 0 -ne 0 ']'
+ diff src/as.stg1.input src/as.stg2.input
+ cp src/as.stg2.input src/as.input
+ rm src/as.stg1.input
+ rm src/as.stg2.input
```

It compiles the source `src/as.s` using binary `src/as.input` into a new binary `src/as.stg1.input`, then uses the new binary to compile the same source again into yet another binary `src/as.stg2.input`. If everything works, the two generated binaries should be the same.

The shell script uses `vm.sh` to run the Intcode VM, so if you are using your own VM, update the `vm.sh` to point to it. By default, `vm.sh` points to the TypeScript based VM in `vm` subdirectory.

What can it do?
-----------------

In addition to supporting all Intcode features specified in [Day 9](https://adventofcode.com/2019/day/9), the assembler also supports:

- Symbols that can be used instead of numeric addresses.
- Stack based on relative addressing mode.
- Pseudo-instructions cal and ret to simplify using functions.
- Directives .FRAME and .ENDFRAME to simplify using local variables.
- Pseudo-instructions db and ds to define memory contents directly.

More detailed description of the language can be found in [doc/Language.md](doc/Language.md). Language grammar is available in [doc/grammar.txt](doc/grammar.txt).

But why?
--------

[Because it's there](https://en.wikiquote.org/wiki/George_Mallory).
Also, I heard you liked assembler, so I assembled an assembler in assembler, so you can assemble while you assemble.

![Xzibit](https://i.kym-cdn.com/photos/images/small/000/001/122/xzibit-happy.jpg)
