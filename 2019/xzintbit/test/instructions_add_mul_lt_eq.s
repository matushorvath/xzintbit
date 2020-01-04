# immediate mode
add 0, 1, [2]

# position mode
mul 0, [1], [2]
lt  [0], 1, [2]
eq  [0], [1], [2]

# relative mode
add 0, [rb - 1], [2]
mul [rb + 0], 1, [2]
lt  0, [1], [rb + 2]
eq  [rb - 0], [rb + 1], [rb + 2]

global0:
+1 = global1:

# immediate with global symbols
add 0, global0, [2]
mul global1 + 42, 1, [2]

# position mode with global symbols
lt  0, [global0 + 42], [2]
eq  [global1], 1, [2]
add [0], [1], [global1 - 77]

# relative mode with global symbols
# TODO should this work?
mul 0, [rb + global0], [2]
lt  [rb + global1 - 42], 1, [2]
eq  [0], [1], [rb + global0 - 77]

.FRAME frame0; frame1; frame2

# immediate with frame symbols
add 0, frame0, [2]
mul frame1 + 42, 1, [2]
lt  frame2, 1, [2]

# position mode with frame symbols
# TODO should this work?
lt  0, [frame0 + 42], [2]
eq  [frame1], 1, [2]
add [0], [1], [frame2 - 77]

# relative mode with frame symbols
mul 0, [rb + frame0], [2]
lt  [rb + frame1 - 42], 1, [2]
eq  [0], [1], [rb + frame2 - 77]

.ENDFRAME

.EOF
