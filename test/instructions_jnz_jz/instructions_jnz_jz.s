# immediate mode
jnz 0, 1

# position mode
jz  0, [1]
jnz [0], 1
jz  [0], [1]

# relative mode
jz  0, [rb - 1]
jnz [rb], 1
jz  [1], [rb + 2]

global0:
+1 = global1:

# immediate with global symbols
jnz 0, global0
jz  global1 + 42, 1

# position mode with global symbols
jnz 0, [global0 + 42]
jz  [global1], [1]

.FRAME frame0; frame1; frame2

# immediate with frame symbols
jz  0, frame0
jnz frame1 + 42, 1
jz  frame1 - 42, [global0]

# relative mode with frame symbols
jz  0, [rb + frame0]
jnz [rb + frame1 - 42], [1]

.ENDFRAME

.EOF
