Language Description
====================

Basics
------

Programs are case sensitive. The assembler expects unix-like line ends (character 10).

Comments start with `#` and continue to the end of the line. Numbers are decimal.

Maximum identifier length is 47 characters. Maximum string length is 49 characters.

Since there is no way to detect the end of file in Intcode, each program must end with a line like this:
```asm
.EOF
```
If you are not getting any output from the assembler, missing `.EOF` at the end of file is the most likely reason.

Instruction Modes
-----------------

Intcode supports three instruction modes:
- position: `out [42]` outputs the value at address 42
- immediate: `out 42` outputs the value 42 itself
- relative: `out [rb + 42]` outputs the value at address rb + 42, where rb is the relative base

Symbols
-------

In addition to numbers, instruction parameters can you can also use symbols:
```asm
    out data
    out [data]
    out [rb + data]
data:
    db  42
.EOF
```

The symbol `data` is in compile time replaced by the memory address of the `data:` label. In the example above, each `out` instruction takes two bytes, so the value of `data` is 2 + 2 + 2 = 6. The program is equivalent to:

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
```

Or:
```json
104,7,4,4,204,9,42
```

Instructions
------------

### Instruction `add a, b, c`
`c = a + b`
```asm
    add 1, [2], [rb + 3]
```
```json
20101,1,2,3
```

### Instruction `mul a, b, c`
`c = a * b`
```asm
    mul 1, [2], [rb + 3]
```
```json
20102,1,2,3
```

### Instruction `in a`
`a = [next character from input stream]`
```asm
    in  [2]
    in  [rb + 3]
```
```json
3,2
203,3
```

### Instruction `out a`
`[next character to output stream] = a`
```asm
    out 1
    out [2]
    out [rb + 3]
```
```json
104,1
4,2
204,3
```

## Instruction `jnz a, b`
`if (a != 0) jump to address b`
```asm
    jnz 1, [10]
    jnz [2], [rb + 20]
    jnz [rb + 3], 30
```
```json
105,1,10
2005,2,20
1205,3,30
```

## Instruction `jz a, b`
`if (a == 0) jump to address b`
```asm
    jz  1, [10]
    jz  [2], [rb + 20]
    jz  [rb + 3], 30
```
```json
106,1,10
2006,2,20
1206,3,30
```

### Instruction `lt a, b, c`
`if (a < b) c = 1; else c = 0`
```asm
    lt  1, [2], [rb + 3]
```
```json
20107,1,2,3
```

### Instruction `eq a, b, c`
`if (a == b) c = 1; else c = 0`
```asm
    eq  1, [2], [rb + 3]
```
```json
20108,1,2,3
```

### Instruction `arb a`
`rb += a` (where `rb` is the relative base)
```asm
    arb 1
    arb [2]
    arb [rb + 3]
```
```json
109,1
9,2
209,3
```

### Instruction `hlt`
halt execution
```asm
    hlt
```
```json
99
```

TODO
----
```
+3 = data
ds
.FRAME .ENDFRAME, param local magic
rb = stack
char is number
how to do pointers
how to do function call, return value, params
```