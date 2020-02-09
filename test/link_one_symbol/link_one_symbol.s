jz  0, code
ds  8, 0

code:
out [external_symbol + 3]
.IMPORT external_symbol
out 10

.EOF
