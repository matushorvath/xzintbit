.EXPORT module_head
.EXPORT module_tail
.EXPORT symbol_head
.EXPORT symbol_tail

##########
# globals

# loaded modules
module_head:
    db 0
module_tail:
    db 0

# included symbols
symbol_head:
    db 0
symbol_tail:
    db 0

.EOF
