ds  11, 0

out [external_symbol + 3]
.IMPORT external_symbol
out 10

out external_symbol
out [external_symbol]
out external_symbol + 5
out [external_symbol + 67]

circular_symbol:
    db  'C'
.EXPORT circular_symbol

out circular_symbol
out [circular_symbol]
out circular_symbol + 5
out [circular_symbol + 67]

.EOF
