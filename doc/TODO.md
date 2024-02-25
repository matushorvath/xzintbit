- Compiler should optionally store all symbols in the .o file, not just exported/imported.
  This would be used for debugging purposes. Linker should then store those symbols in the map.
- Handle frame symbols somehow, pass them through the .o file into the map, for debugging.

- Fix linker with zero length object files, it should add zero bytes to the binary but it adds one.
