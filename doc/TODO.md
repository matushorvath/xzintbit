- Compiler should optionally store all symbols in the .o file, not just exported/imported.
  This would be used for debugging purposes. Linker should then store those symbols in the map.
- Handle frame symbols somehow, pass them through the .o file into the map, for debugging.

- Support exportable .SYMBOL that will not be relocated?

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

- Create a preprocessor with macro support to solve code duplication everywhere.

- Profiling:
  - https://code.visualstudio.com/docs/nodejs/profiling#_analyzing-a-profile
  - https://chromedevtools.github.io/devtools-protocol/tot/Profiler/#type-Profile
  - https://stackoverflow.com/questions/26981155/google-chrome-developer-tools-profiling-results-file-format
  - https://kcachegrind.github.io/html/Home.html
  - https://github.com/lahmatiy/cpupro
  - https://profilerpedia.markhansen.co.nz/formats/chrome-javascript-profiler-cpuprofile/

- test/link_empty_code has found a bug - the object with no code has a random address assigned,
  it should instead have the same address as the object right after it (or, since it is last,
  past the object right before it)
  currently the imports: - module number is some random garbage that keeps changing
