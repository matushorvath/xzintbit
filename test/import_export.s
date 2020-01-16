db  42
ds  5, 0

symbol1:
.EXPORT symbol0
symbol0:
.EXPORT symbol1

.IMPORT symbol2
db  symbol2 + 1
db  symbol3 + 2
.IMPORT symbol3
out [symbol2 - 7]

.EOF
