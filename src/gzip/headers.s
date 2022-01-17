.EXPORT read_member

# from error.s
.IMPORT report_error

# from libxib/bits.s
.IMPORT is_bit

##########
read_member:
.FRAME tmp, flg, idx
    arb -3

    # read ID
    in  [rb + tmp]
    eq  [rb + tmp], 31, [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    in  [rb + tmp]
    eq  [rb + tmp], 139, [rb + tmp]
    jz  [rb + tmp], read_member_bad_id

    # read CM (compression method)
    in  [rb + tmp]
    eq  [rb + tmp], 8, [rb + tmp]
    jz  [rb + tmp], read_member_bad_method

    # read FLG and check that reserved bits are zero
    in  [rb + flg]
    lt  [rb + flg], 32, [rb + tmp]
    jz  [rb + tmp], read_member_reserved

    # read MTIME
    in  [rb + tmp]
    in  [rb + tmp]
    in  [rb + tmp]
    in  [rb + tmp]

    # read XFL and OS
    in  [rb + tmp]
    in  [rb + tmp]

    # if FLG.FEXTRA is set, process extra fields
    add [rb + flg], 0, [rb - 1]
    add 2, 0, [rb - 2]
    arb -2
    call is_bit
    jz  [rb - 4], read_member_fextra_done

    # load XLEN to idx
    in  [rb + idx]
    in  [rb + tmp]
    mul [rb + tmp], 256, [rb + tmp]
    add [rb + idx], [rb + tmp], [rb + idx]

read_member_fextra_read:
    # read XLEN bytes
    jz  [rb + idx], read_member_fextra_done
    in  [rb + tmp]
    add [rb + idx], -1, [rb + idx]

    jz  0, read_member_fextra_read

read_member_fextra_done:

    # if FLG.FNAME is set, process file name
    add [rb + flg], 0, [rb - 1]
    add 3, 0, [rb - 2]
    arb -2
    call is_bit
    jz  [rb - 4], read_member_fname_done

read_member_fname_read:
    # read zero-terminated string
    in  [rb + tmp]
    out [rb + tmp] # TODO remove
    jnz [rb + tmp], read_member_fname_read

read_member_fname_done:

    # if FLG.FCOMMENT is set, process comment
    add [rb + flg], 0, [rb - 1]
    add 4, 0, [rb - 2]
    arb -2
    call is_bit
    jz  [rb - 4], read_member_fcomment_done

read_member_fcomment_read:
    # read zero-terminated string
    in  [rb + tmp]
    out [rb + tmp] # TODO remove
    jnz [rb + tmp], read_member_fcomment_read

read_member_fcomment_done:

    # if FLG.FHCRC is set, process CRC16
    add [rb + flg], 0, [rb - 1]
    add 1, 0, [rb - 2]
    arb -2
    call is_bit
    jz  [rb - 4], read_member_fhcrc_done

    in  [rb + tmp]
    in  [rb + tmp]

read_member_fhcrc_done:
    arb 3
    ret 0

read_member_bad_id:
    add err_bad_id, 0, [rb]
    call report_error

read_member_bad_method:
    add err_bad_method, 0, [rb]
    call report_error

read_member_reserved:
    add err_reserved, 0, [rb]
    call report_error
.ENDFRAME

##########
# error messages

err_bad_id:
    db  "Invalid ID bytes in GZIP header", 0
err_bad_method:
    db  "Invalid compression method, expecting 'deflate'", 0
err_reserved:
    db  "Non-zero reserved bits in GZIP header", 0

.EOF
