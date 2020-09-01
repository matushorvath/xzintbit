# End the frame that was started in include.si
# This will detect if include.si was actually included
.ENDFRAME

# Intentionally cause a compile error, we expect the line number to be correct
.NONEXISTENT

.EOF
