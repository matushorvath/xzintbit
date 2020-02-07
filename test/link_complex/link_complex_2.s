.EXPORT external_symbol

ds  17, 0

external_symbol:
    db  "E"

out external_symbol
out [external_symbol]
out external_symbol + 5
out [external_symbol + 7]

reloc_symbol:
    db  'R'

out reloc_symbol
out [reloc_symbol]
out reloc_symbol + 5
out [reloc_symbol + 7]

.IMPORT circular_symbol

out circular_symbol
out [circular_symbol]
out circular_symbol + 5
out [circular_symbol + 7]

.EOF
