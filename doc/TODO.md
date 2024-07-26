- Compiler should optionally store all symbols in the .o file, not just exported/imported.
  This would be used for debugging purposes. Linker should then store those symbols in the map.
- Handle frame symbols somehow, pass them through the .o file into the map, for debugging.

- In print_code don't print empty line after .C if there is no code. This is how we hande .R, .I and .E.
  In load_code handle the missing empty line by doing a peek_input, same as we do for .R, .I and .E.
  This probably detecting no memory to print and skip the out 10 just before print_code calls print_mem.

- Make all tests that use the linker run ldmap as well.

- Support exportable .SYMBOL that will not be relocated?

- Map should not include zero modules (that are not mapped to the binary)

- Optimize ret: Usually this generates two 'arb' at the end of each function.
  Instead make ret check if last instruction was an arb and just increase the number if so.

- Consider a more powerful call syntax, something like
    .CALL function, param1, param2, param3...
  instead of
    add [...], 0, [rb - 1]
    add [...], 0, [rb - 2]
    add [...], 0, [rb - 3]
    ...
    arb -N
    call function

- Consider a more powerful function prologue/epilogue syntax, something like
    .FUNCTION function; a, b, c; d, e
      ...
      .RETURN
    .ENDFUNCTION
  instead of
    function:
    .FRAME a, b, c; d, e
      arb -2
      ...
      arb 2
      ret 3
    .ENDFRAME

- Split bin2obj.s to multiple sources, perhaps rename /tools to /bin2obj.
- Create a preprocessor with macro support to solve code duplication everywhere.

- Objects from a library that don't get included still have an entry in the map.yaml (try to link empty source with any .a)
