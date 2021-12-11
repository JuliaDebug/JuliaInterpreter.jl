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
Test run at: 2021-12-12T00:11:31.538

Maximum number of statements per lowered expression: 100000

| Test file | Passes | Fails | Errors | Broken | Aborted blocks |
| --------- | ------:| -----:| ------:| ------:| --------------:|
| ambiguous | 105 | 0 | 1 | 2 | 14 |
| compiler/inference | 997 | 5 | 2 | 2 | 3 |
| compiler/validation | 24 | 0 | 1 | 0 | 0 |
| compiler/ssair | X | X | X | X | X |
| compiler/irpasses | X | X | X | X | X |
| compiler/codegen | X | X | X | X | X |
| compiler/inline | 63 | 2 | 0 | 1 | 0 |
| compiler/contextual | 9 | 2 | 1 | 0 | 1 |
| subarray | 318276 | 0 | 1 | 0 | 5 |
| strings/basic | 87674 | 0 | 0 | 0 | 5 |
| strings/search | 692 | 0 | 0 | 0 | 1 |
| strings/util | 619 | 0 | 0 | 0 | 6 |
| strings/io | 12764 | 0 | 0 | 0 | 2 |
| strings/types | 2302691 | 0 | 0 | 0 | 4 |
| unicode/utf8 | 19 | 0 | 0 | 0 | 0 |
| core | X | X | X | X | X |
| atomics | 3444 | 0 | 0 | 0 | 0 |
| keywordargs | 151 | 0 | 0 | 0 | 0 |
| numbers | 1577509 | 0 | 1 | 1 | 16 |
| subtype | X | X | X | X | X |
| char | 1625 | 0 | 0 | 0 | 3 |
| triplequote | 29 | 0 | 0 | 0 | 0 |
| intrinsics | 301 | 0 | 0 | 0 | 0 |
| dict | X | X | X | X | X |
| hashing | 12521 | 0 | 0 | 0 | 7 |
| iobuffer | 205 | 0 | 0 | 0 | 1 |
| staged | 58 | 6 | 0 | 0 | 0 |
| offsetarray | X | X | X | X | X |
| arrayops | 1996 | 2 | 5 | 2 | 24 |
| tuple | 606 | 0 | 0 | 0 | 0 |
| reduce | 8578 | 2 | 0 | 0 | 13 |
| reducedim | 865 | 0 | 0 | 0 | 14 |
| abstractarray | 55292 | 0 | 1 | 24795 | 8 |
| intfuncs | 215863 | 0 | 1 | 0 | 6 |
| simdloop | X | X | X | X | X |
| vecelement | 678 | 0 | 0 | 0 | 1 |
| rational | 98633 | 0 | 1 | 1 | 7 |
| bitarray | 381657 | 0 | 3661 | 0 | 23 |
| copy | 331 | 0 | 2 | 0 | 2 |
| math | X | X | X | X | X |
| fastmath | 946 | 0 | 0 | 0 | 1 |
| functional | 98 | 0 | 0 | 0 | 0 |
| iterators | 10080 | 0 | 0 | 0 | 4 |
| operators | 13039 | 0 | 0 | 0 | 2 |
| ordering | 35 | 0 | 0 | 0 | 0 |
| path | 1051 | 0 | 0 | 12 | 2 |
| ccall | X | X | X | X | X |
| parse | 16098 | 0 | 0 | 0 | 4 |
| loading | 153389 | 2 | 1 | 0 | 15 |
| gmp | 2323 | 1 | 0 | 0 | 6 |
| sorting | 12649 | 0 | 8 | 10 | 8 |
| spawn | X | X | X | X | X |
| backtrace | 8 | 11 | 17 | 1 | 2 |
| exceptions | 38 | 24 | 7 | 0 | 3 |
| file | X | X | X | X | X |
| read | 3733 | 0 | 0 | 0 | 3 |
| version | 2452 | 0 | 0 | 0 | 1 |
| namedtuple | 214 | 0 | 0 | 0 | 1 |
| mpfr | 1099 | 13 | 15 | 1 | 6 |
| broadcast | 507 | 1 | 0 | 0 | 6 |
| complex | 8432 | 0 | 0 | 5 | 7 |
| floatapprox | 49 | 0 | 0 | 0 | 0 |
| reflection | X | X | X | X | X |
| regex | 128 | 0 | 2 | 0 | 1 |
| float16 | 762091 | 0 | 0 | 0 | 1 |
| combinatorics | 170 | 0 | 0 | 0 | 2 |
| sysinfo | 4 | 0 | 0 | 0 | 0 |
| env | 96 | 0 | 1 | 0 | 4 |
| rounding | 112720 | 0 | 0 | 0 | 3 |
| ranges | 12110676 | 5 | 26 | 327699 | 19 |
| mod2pi | 80 | 0 | 0 | 0 | 0 |
| euler | 12 | 0 | 0 | 0 | 6 |
| show | X | X | X | X | X |
| client | 0 | 4 | 0 | 0 | 1 |
| errorshow | X | X | X | X | X |
| sets | X | X | X | X | X |
| goto | 19 | 0 | 0 | 0 | 0 |
| llvmcall | X | X | X | X | X |
| llvmcall2 | 7 | 0 | 0 | 0 | 0 |
| ryu | 31194 | 0 | 21 | 0 | 1 |
| some | 71 | 0 | 0 | 0 | 0 |
| meta | 66 | 2 | 1 | 0 | 0 |
| stacktraces | X | X | X | X | X |
| docs | X | X | X | X | X |
| misc | X | X | X | X | X |
| threads | X | X | X | X | X |
| stress | 0 | 0 | 0 | 0 | 0 |
| binaryplatforms | 334 | 0 | 7 | 0 | 7 |
| atexit | 40 | 0 | 0 | 0 | 1 |
| enums | 99 | 0 | 0 | 0 | 0 |
| cmdlineargs | X | X | X | X | X |
| int | X | X | X | X | X |
| interpreter | 3 | 0 | 0 | 0 | 0 |
| checked | 1239 | 0 | 0 | 0 | 1 |
| bitset | 195 | 0 | 0 | 0 | 3 |
| floatfuncs | 208 | 0 | 13 | 0 | 2 |
| precompile | X | X | X | X | X |
| boundscheck | X | X | X | X | X |
| error | 31 | 0 | 0 | 0 | 0 |
| cartesian | 236 | 0 | 1 | 3 | 3 |
| osutils | 57 | 0 | 0 | 0 | 0 |
| channels | X | X | X | X | X |
| iostream | 50 | 0 | 0 | 0 | 1 |
| secretbuffer | X | X | X | X | X |
| specificity | X | X | X | X | X |
| reinterpretarray | 232 | 0 | 0 | 0 | 3 |
| syntax | X | X | X | X | X |
| corelogging | 55 | 2 | 21 | 0 | 1 |
| missing | 563 | 0 | 2 | 1 | 3 |
| asyncmap | X | X | X | X | X |
| smallarrayshrink | 36 | 0 | 0 | 0 | 0 |
| opaque_closure | X | X | X | X | X |
| filesystem | 4 | 0 | 0 | 0 | 0 |
| download | X | X | X | X | X |
