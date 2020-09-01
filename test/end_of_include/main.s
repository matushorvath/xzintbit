# This will detect if include.si was actually included
out     [first_included]

# Intentionally cause a compile error, we expect the line number to be correct
out               [nonexistent]

# This will detect if include.si was actually included
out                         [last_included]

.EOF
