db  42

symbol1:
.EXPORT symbol0

ds  5, 0

symbol0:
.EXPORT symbol1

.SYMBOL constant 123

.IMPORT symbol2
db  symbol2 + 1
db  symbol3 + 2
.IMPORT symbol3
out [symbol2 - 7]

local0:
    out local0 + 1
    out [local0 + 2]

.EOF
