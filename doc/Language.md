Language Description
====================

Basics
------

Programs are case sensitive. The assembler expects Unix-like line ends (character 10).
Comments start with `#` and continue to the end of the line. Numbers are decimal.
Maximum identifier length is 47 characters. Maximum string length is 49 characters.

Since there is no way to detect end of input file from Intcode, each program must end with a line like this:
```asm
.EOF
```
If you are not getting any output from the assembler, missing `.EOF` at the end of your program is the most likely reason.

Instruction Modes
-----------------

Intcode supports three instruction modes:
- position: `out [42]` outputs the value at address 42
- immediate: `out 42` outputs the value 42 itself
- relative: `out [rb + 42]` outputs the value at address rb + 42, where rb is the relative base

Characters as Numbers
---------------------

You can use characters in most places where a number would be accepted:

```asm
    out 'x'
```
Is equivalent to:
```asm
    out 120        # ASCII code of "x" is 120
```

Global Symbols
--------------

### Symbol Basics

In addition to numbers, instruction parameters also support symbols:
```asm
    out data
    out [data]
    out [rb + data]
data:
    db  42
.EOF
```

The symbol `data` is replaced at compile time by the memory address of the `data:` label. In the example above, each `out` instruction takes two bytes, so the value of `data` is 2 + 2 + 2 = 6. The program is equivalent to:

```asm
    out 6
    out [6]
    out [rb + 6]
    db  42
.EOF
```

Or:
```json
104,6,4,6,204,6,42
```

### Symbols with Numbers

Symbols can be combined with numbers:

```asm
    out data + 1
    out [data - 2]
    out [rb + data + 3]
data:
    db  42
.EOF
```

Which is equivalent to:

```asm
    out 7
    out [4]
    out [rb + 9]
data:
    db  42
.EOF
```

Or:
```json
104,7,4,4,204,9,42
```

# Relative Symbols

Symbols can also be defined to point to the middle of an instruction. This can be useful for self-modifying code:

```asm
    add [ptr], 0, [tmp]
+3 = tmp:
    add 42, 0, [0]
ptr:
    db  13
.EOF
```

The compiled Intcode is:
```json
1001,8,0,7,1101,42,0,0,13
```

This is the pattern used to dereference pointers. Value of the `tmp` symbol is the memory address 3 bytes forward. In other words, it points to the 3ʳᵈ parameter of the following instruction.

Let's say the value stored at address `ptr` is 13, and we want to set the value at address 13 to 42. If we used C, this is what we want to do:

```c
int *ptr = 13;
*ptr = 42;
```

The first `add` instruction will read the 13 from address `ptr` and use it to rewrite the 3ʳᵈ argument of the second `add` instruction.
The second `add` instruction will write the value 42 to memory location 13.

An alternative way to write this would be:

```asm
    add [ptr], 0, [tmp + 3]
tmp:
    add 42, 0, [0]
ptr:
    db  13
.EOF
```

Another alternative way is to use the pseudo-symbol `ip` which refers to the address of next instruction:

```asm
    add [ptr], 0, [ip + 3]
    add 42, 0, [0]
ptr:
    db  13
.EOF
```

Instructions
------------

### Instruction `add a, b, c`
Performs `c = a + b`.

```asm
    add 1, [2], [rb + 3]
```
Compiles to:
```json
20101,1,2,3
```

### Instruction `mul a, b, c`
Performs `c = a * b`.

```asm
    mul 1, [2], [rb + 3]
```
Compiles to:
```json
20102,1,2,3
```

### Instruction `in a`
Inputs a character and stores it in `a`.

```asm
    in  [2]
    in  [rb + 3]
```
Compiles to:
```json
3,2
203,3
```

### Instruction `out a`
Outputs `a` as a character.

```asm
    out 1
    out [2]
    out [rb + 3]
```
Compiles to:
```json
104,1
4,2
204,3
```

## Instruction `jnz a, b`
Jumps to address `b` if `a != 0`.

```asm
    jnz 1, [10]
    jnz [2], [rb + 20]
    jnz [rb + 3], 30
```
Compiles to:
```json
105,1,10
2005,2,20
1205,3,30
```

## Instruction `jz a, b`
Jumps to address `b` if `a == 0`.

```asm
    jz  1, [10]
    jz  [2], [rb + 20]
    jz  [rb + 3], 30
```
Compiles to:
```json
106,1,10
2006,2,20
1206,3,30
```

### Instruction `lt a, b, c`
Performs `if (a < b) c = 1; else c = 0`.

```asm
    lt  1, [2], [rb + 3]
```
Compiles to:
```json
20107,1,2,3
```

### Instruction `eq a, b, c`
Performs `if (a == b) c = 1; else c = 0`.

```asm
    eq  1, [2], [rb + 3]
```
Compiles to:
```json
20108,1,2,3
```

### Instruction `arb a`
Performs `rb += a` (where `rb` is the relative base).

```asm
    arb 1
    arb [2]
    arb [rb + 3]
```
Compiles to:
```json
109,1
9,2
209,3
```

### Instruction `hlt`
Halts program execution.

```asm
    hlt
```
Compiles to:
```json
99
```

Pseudo-instructions
-------------------

Pseudo-instructions look like instructions in the code, but do not directly correspond to an Intcode instruction.

### Pseudo-instruction `db`
Define memory contents directly. Supports one or more arguments. Each argument can be a number, character, symbol or a string.

```asm
    db 42
    db 'x', "a string", 0, data
data:
```
Compiles to:
```json
42,120,97,32,115,116,114,105,110,103,0,12
```

In the compiled Intcode, notice that the string was not zero-terminated on its own, we had to explicitly zero-terminate it. We can also see that the `data` symbol corresponds to memory address 12.

### Pseudo-instruction `ds`
Define memory contents directly. Has two arguments, *count* and *value*, and it causes *count* copies of *value* to be stored in memory. The value can be a number of a character only (no symbols or strings).

```asm
    ds 7, 42
```
Compiles to:
```json
42,42,42,42,42,42,42
```

### Pseudo-instructions `call` and `ret`

These instructions support function calls. They generate multiple actual Intcode instructions at compile time. Essentially these are just syntactic sugar. You could implement functions without `call` and `ret`, it would just be less convenient.

```asm
    call my_function
my_function:
    out 'A'
    ret 0
```
Compiles to:
```json
21101,9,0,-1, 109,-1, 1106,0,9,
104,65,
109,1, 2106,0,-1
```

Functions are described in detail [below](#stack-and-functions).

Stack and Functions
-------------------

Stack and function features are what really makes the 60kB assembler source code managable. These features are not strictly necessary, you can write assembly programs while completely ignoring them, but for anything larger than small it's really helpful to be able to structure your code into callable functions.

### Stack Convention

To implement the stack, we use the relative mode of Intcode instructions. The relative base (`rb`) is the stack pointer, and local variables are addressed relative to `rb`.
Stack grows in the negative direction as is usual on other architectures, and stack pointer points to the item on top of the stack.

This is how you initialize the stack in your program:
```asm
    arb stack       # initialize stack pointer

    ...             # your code and data goes here

    ds 50, 0        # 50 bytes of zero-initialized stack space
stack:              # initial value for stack pointer
```

This is how you push a value to stack:
```asm
    add 42, 0, [rb - 1]
    arb -1
```

First instruction stores the value 42 just below the stack pointer (`rb`). Second instruction decrements the stack pointer.

This is how you pop a value from stack:
```asm
    out [rb]
    arb 1
```

### Stack Frames

In order to access local variables on stack, it is convenient to be able to assign symbols to them. This allows you to write `[rb + my_variable]` instead of `[rb + 7]` even for local variables.

The language has two directives to support this, `.FRAME` and `.ENDFRAME`.
The `.FRAME` directive assigns symbols to stack offsets. These symbols are special, they only exist until the next `.ENDFRAME` directive.

For example, this is a frame with two local variables:
```asm
.FRAME var_a, var_b
    out [rb + var_a]
    out [rb + var_b]
.ENDFRAME
```

The equivalent code without `.FRAME` and `.ENDFRAME` would be:
```asm
    out [rb + 1]
    out [rb]
```

You can see that local variables are stored on stack like this:
```
rb -> var_b
      var_a
...
(bottom of the stack)
```

There can be only one `.FRAME` active at one time. In other words, a `.FRAME` needs to be followed by an `.ENDFRAME` before another `.FRAME` can be started.

### Function Parameters and Local Variables

The `.FRAME` directive actually does more than just local variables. It in fact supports up to three symbol lists separated by semicolons:

```asm
.FRAME param0, param1; local0, local1, local2; tmp0, tmp1
```

If there are only two lists, it's assumed that the `tmp` list is empty. If there is just one list, it's assumed that the `param` list is empty as well.

The first list (`param0`, `param1`) creates symbols for function parameters. The second list (`local0`, `local1`, `local2`) creates symbols for local variables. The third list (`tmp0`, `tmp1`) creates symbols that exist below the stack pointer (outside of stack).

The values of these symbols will be 5, 4, 2, 1, 0, -1, -2. The offset of 3 between function parameters and local variables is skipped, as that will be where the return address is stored on stack when calling a function.

Memory in this stack frame will be laid out like this:

```
      tmp1
      tmp0
rb -> local2
      local1
      local0
      (reserved for return address)
      param1
      param0
...
(bottom of the stack)
```

### Function Call Convention

Function calls are vaguely inspired by x86 *thiscall*. Function parameters are pushed to stack and cleaned up by callee, returned values are stored below the stack pointer after callee returns.

A function call with 2 parameters and a return value looks like this:
```asm
    # push two parameters to stack
    add 'H', 0, [rb - 1]
    add 'i', 1, [rb - 2]
    arb -2

    # call the function
    call my_function

    # output the value returned by my_function
    # 4 = 2 + number of function parameters
    out [rb - 4]

my_function:
.FRAME param0, param1; var0
    # reserve stack space for 1 local variable, var0
    arb -1

    # output the two parameters
    out [rb + param0]
    out [rb + param1]

    # our first local variable will be what caller sees as the return value
    add '!', 0, [rb + var0]

    # remove var0 from stack
    arb 1

    # return to caller, specifying that 2 function parameters
    # should be popped from stack, param0 and param1
    ret 2
.ENDFRAME
```

The equivalent code without the syntactic sugar would be:

```asm
    # push two parameters to stack
    add 'H', 0, [rb - 1]
    add 'i', 1, [rb - 2]
    arb -2

    # push return address to stack
    add return_address, 0, [rb - 1]
    arb -1

    # jump to my_function
    jz  0, my_function
return_address:

    # output the value returned by my_function
    # 4 = 2 + number of function parameters
    out [rb - 4]

my_function:
    # reserve stack space for 1 local variable, var0
    arb -1

    # output the two parameters
    out [rb + 3]
    out [rb + 2]

    # our first local variable will be what caller sees as the return value
    add '!', 0, [rb]

    # remove var0 from stack
    arb 1

    # remove 2 function parameters and return address from stack
    arb 3

    # jump to return address
    jz  0, [rb - 3]
```

Or in Intcode:
```json
21101,72,0,-1,
21101,105,1,-2,
109,-2,
21101,19,0,-1, 109,-1, 1106,0,21,
204,-4,

109,-1,
204,3,
204,2,
21101,33,0,0,
109,-1,
109,3, 2106,0,-3
```

Stack layout just before `call my_function`:
```
rb -> 'i'
      'H'
...
(bottom of the stack)
```

Stack layout in `my_function` after `arb -1`:
```
rb -> 
      (return address)
      'i'
      'H'
...
(bottom of the stack)
```

Stack layout in `my_function` just before `ret 0`:
```
      '!'
rb -> (return address)
      'i'
      'H'
...
(bottom of the stack)
```

Stack layout after `my_function` returns:
```
      '!'
      (return address)
      'i'
      'H'
rb ->
...
(bottom of the stack)
```

Here you can see why return value is at location `[rb - 4]` from caller's point of view, and that it corresponds to the first local variable of the callee.
