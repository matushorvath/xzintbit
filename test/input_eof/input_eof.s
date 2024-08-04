# test the standard instruction in under a standard VM
# see also extended_vm_in

    # read 4 characters using in [char]
    # only 3 characters are available, so the VM should crash

    in  [char]
    eq  [char], 'A', [tmp]
    jz  [tmp], fail

    in  [char]
    eq  [char], 'B', [tmp]
    jz  [tmp], fail

    in  [char]
    eq  [char], 'C', [tmp]
    jz  [tmp], fail

    in [char]

    # expecting the VM to exit here, with "no more inputs" on stderr

fail:
    out 'f'
    out 'a'
    out 'i'
    out 'l'
    hlt

char:
    db  0
tmp:
    db  0

.EOF
