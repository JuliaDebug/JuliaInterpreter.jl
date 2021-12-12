Julia Version 1.7.0
Commit 3bf9d17731 (2021-11-30 12:12 UTC)
Platform Info:
  OS: Windows (x86_64-w64-mingw32)
  CPU: Intel(R) Core(TM) i7-10710U CPU @ 1.10GHz
  WORD_SIZE: 64
  LIBM: libopenlibm
  LLVM: libLLVM-12.0.1 (ORCJIT, skylake)
Environment:
  JULIA_EDITOR = code
  JULIA_NUM_THREADS = 5
Test run at: 2021-12-12T01:39:18.353

Maximum number of statements per lowered expression: 10000000

| Test file | Passes | Fails | Errors | Broken | Aborted blocks |
| --------- | ------:| -----:| ------:| ------:| --------------:|
| ambiguous | 105 | 0 | 1 | 2 | 14 |
| compiler/inference | 1003 | 5 | 2 | 2 | 3 |
| compiler/validation | 24 | 0 | 1 | 0 | 0 |
| compiler/ssair | X | X | X | X | X |
| compiler/irpasses | 25 | 0 | 0 | 0 | 1 |
| compiler/codegen | X | X | X | X | X |
| compiler/inline | 63 | 2 | 0 | 1 | 0 |
| compiler/contextual | 9 | 2 | 1 | 0 | 0 |
| subarray | 326868 | 0 | 0 | 0 | 2 |
| strings/basic | 87674 | 0 | 0 | 0 | 2 |
| strings/search | 692 | 0 | 0 | 0 | 0 |
| strings/util | 619 | 0 | 0 | 0 | 0 |
| strings/io | 12764 | 0 | 0 | 0 | 1 |
| strings/types | 2302691 | 0 | 0 | 0 | 2 |
| unicode/utf8 | 19 | 0 | 0 | 0 | 0 |
| core | X | X | X | X | X |
| worlds | X | X | X | X | X |
| atomics | 3444 | 0 | 0 | 0 | 0 |
| keywordargs | 151 | 0 | 0 | 0 | 0 |
| numbers | 1622626 | 0 | 0 | 1 | 8 |
| subtype | X | X | X | X | X |
| char | 1623 | 0 | 0 | 0 | 0 |
| triplequote | 29 | 0 | 0 | 0 | 0 |
| intrinsics | 301 | 0 | 0 | 0 | 0 |
| dict | X | X | X | X | X |
| hashing | 12519 | 0 | 0 | 0 | 1 |
| iobuffer | 205 | 0 | 0 | 0 | 0 |
| staged | 58 | 6 | 0 | 0 | 0 |
| offsetarray | 465 | 11 | 6 | 3 | 1 |
| arrayops | 2013 | 0 | 0 | 2 | 3 |
| tuple | 606 | 0 | 0 | 0 | 0 |
| reduce | X | X | X | X | X |
| reducedim | 865 | 0 | 0 | 0 | 1 |
| abstractarray | 1838 | 0 | 0 | 0 | 3 |
| intfuncs | 215864 | 0 | 0 | 0 | 1 |
| simdloop | 240 | 0 | 0 | 0 | 0 |
| vecelement | 678 | 0 | 0 | 0 | 0 |
| rational | 98634 | 0 | 0 | 1 | 1 |
| bitarray | 652056 | 0 | 3658 | 0 | 14 |
| copy | 533 | 0 | 0 | 0 | 0 |
| math | 148737 | 0 | 1 | 0 | 3 |
| fastmath | 946 | 0 | 0 | 0 | 0 |
| functional | 98 | 0 | 0 | 0 | 0 |
| iterators | 10080 | 0 | 0 | 0 | 2 |
| operators | 13039 | 0 | 0 | 0 | 0 |
| ordering | 35 | 0 | 0 | 0 | 0 |
| path | 1295 | 0 | 0 | 12 | 1 |
| ccall | X | X | X | X | X |
| parse | 16098 | 0 | 0 | 0 | 1 |
| loading | X | X | X | X | X |
| gmp | 2323 | 1 | 0 | 0 | 1 |
| sorting | 12705 | 0 | 3 | 10 | 4 |
| spawn | 209 | 1 | 1 | 4 | 0 |
| backtrace | 8 | 11 | 17 | 1 | 0 |
| exceptions | 38 | 25 | 7 | 0 | 0 |
| file | X | X | X | X | X |
| read | X | X | X | X | X |
| version | 2452 | 0 | 0 | 0 | 0 |
| namedtuple | 214 | 0 | 0 | 0 | 0 |
| mpfr | 1127 | 0 | 0 | 1 | 0 |
| broadcast | 507 | 1 | 0 | 0 | 2 |
| complex | 8432 | 0 | 0 | 5 | 1 |
| floatapprox | 49 | 0 | 0 | 0 | 0 |
| reflection | X | X | X | X | X |
| regex | 130 | 0 | 0 | 0 | 0 |
| float16 | 762091 | 0 | 0 | 0 | 1 |
| combinatorics | 170 | 0 | 0 | 0 | 0 |
| sysinfo | 4 | 0 | 0 | 0 | 0 |
| env | 105 | 0 | 0 | 0 | 0 |
| rounding | 112720 | 0 | 0 | 0 | 1 |
| ranges | 12123407 | 254 | 0 | 329460 | 4 |
| mod2pi | 80 | 0 | 0 | 0 | 0 |
| euler | 12 | 0 | 0 | 0 | 2 |
| show | X | X | X | X | X |
| client | 0 | 4 | 0 | 0 | 0 |
| errorshow | X | X | X | X | X |
| sets | 3529 | 0 | 0 | 1 | 1 |
| goto | 19 | 0 | 0 | 0 | 0 |
| llvmcall | X | X | X | X | X |
| llvmcall2 | 7 | 0 | 0 | 0 | 0 |
| ryu | 31194 | 0 | 21 | 0 | 1 |
| some | 71 | 0 | 0 | 0 | 0 |
| meta | 66 | 2 | 1 | 0 | 0 |
| stacktraces | X | X | X | X | X |
| docs | 235 | 1 | 0 | 0 | 2 |
| misc | X | X | X | X | X |
| threads | X | X | X | X | X |
| stress | 0 | 0 | 0 | 0 | 0 |
| binaryplatforms | 341 | 0 | 0 | 0 | 1 |
| atexit | 40 | 0 | 0 | 0 | 0 |
| enums | 99 | 0 | 0 | 0 | 0 |
| cmdlineargs | 242 | 0 | 0 | 3 | 0 |
| int | 524693 | 0 | 0 | 0 | 1 |
| interpreter | 3 | 0 | 0 | 0 | 0 |
| checked | 1239 | 0 | 0 | 0 | 0 |
| bitset | 195 | 0 | 0 | 0 | 0 |
| floatfuncs | 208 | 0 | 13 | 0 | 2 |
| precompile | X | X | X | X | X |
| boundscheck | X | X | X | X | X |
| error | 31 | 0 | 0 | 0 | 0 |
| cartesian | 238 | 0 | 0 | 3 | 0 |
| osutils | 57 | 0 | 0 | 0 | 0 |
| channels | X | X | X | X | X |
| iostream | 50 | 0 | 0 | 0 | 0 |
| secretbuffer | 27 | 0 | 0 | 0 | 0 |
| specificity | X | X | X | X | X |
| reinterpretarray | 232 | 0 | 0 | 0 | 0 |
| syntax | X | X | X | X | X |
| corelogging | 230 | 1 | 0 | 0 | 0 |
| missing | 563 | 0 | 2 | 1 | 1 |
| asyncmap | 304 | 0 | 0 | 0 | 0 |
| smallarrayshrink | 36 | 0 | 0 | 0 | 0 |
| opaque_closure | X | X | X | X | X |
| filesystem | 4 | 0 | 0 | 0 | 0 |
| download | X | X | X | X | X |
