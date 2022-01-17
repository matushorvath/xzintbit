.EXPORT read_member

# from error.s
.IMPORT report_error

##########
read_member:
.FRAME tmp
    arb -1

    # read id bytes
    in  [rb + tmp]
    eq  [rb + tmp], 31, [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    in  [rb + tmp]
    eq  [rb + tmp], 139, [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    # read compression method
    in  [rb + tmp]
    eq  [rb + tmp], 8, [rb + tmp]
    jz  [rb + tmp], read_member_bad_method

    # check that reserved FLG bits are zero
    in  [rb + tmp]
    #xxxxx lt  [rb + tmp],

    # FEXTRA/XLEN, FNAME, FCOMMENT and FHCRC to skip over the optional fields
    # must give an error indication if any reserved bit is non-zero

    arb 1
    ret 0

read_member_bad_id:
    add err_bad_id, 0, [rb]
    call report_error

read_member_bad_method:
    add err_bad_method, 0, [rb]
    call report_error
.ENDFRAME

##########
# error messages

err_bad_id:
    db  "Invalid ID bytes in GZIP header", 0
err_bad_method:
    db  "Invalid compression method, expecting 'deflate'", 0

.EOF
