.EXPORT read_member

# from error.s
.IMPORT report_error

##########
read_member:
.FRAME tmp
    arb -1

    in  [rb + tmp]
    eq  [rb + tmp], 'G', [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    in  [rb + tmp]
    eq  [rb + tmp], 'Z', [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    arb 1
    ret 0

read_member_bad_id:
    add err_bad_id, 0, [rb]
    call report_error
.ENDFRAME

##########
# error messages

err_bad_id:
    db  "Invalid GZIP header, ID should be 'GZ'", 0

.EOF
