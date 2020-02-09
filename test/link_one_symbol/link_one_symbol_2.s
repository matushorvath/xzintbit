.EXPORT external_symbol

jz  0, code
ds  14, 0

external_symbol:
    db  "___X_Y"

code:
out [external_symbol + 5]

hlt

.EOF
