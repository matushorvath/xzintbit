- Compiler should optionally store all symbols in the .o file, not just exported/imported.
  This would be used for debugging purposes. Linker should then store those symbols in the map.
- Handle frame symbols somehow, pass them through the .o file into the map, for debugging.

- In print_code don't print empty line after .C if there is no code. This is how we hande .R, .I and .E.
  In load_code handle the missing empty line by doing a peek_input, same as we do for .R, .I and .E.
  This probably detecting no memory to print and skip the out 10 just before print_code calls print_mem.
