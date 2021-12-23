Julia Version 1.8.0-DEV.1169
Commit f2f31336a1 (2021-12-22 11:27 UTC)
Platform Info:
  OS: Windows (x86_64-w64-mingw32)
  CPU: Intel(R) Core(TM) i7-10710U CPU @ 1.10GHz
  WORD_SIZE: 64
  LIBM: libopenlibm
  LLVM: libLLVM-12.0.1 (ORCJIT, skylake)
Test run at: 2021-12-23T13:30:26.035

Maximum number of statements per lowered expression: 10000000

| Test file | Passes | Fails | Errors | Broken | Aborted blocks |
| --------- | ------:| -----:| ------:| ------:| --------------:|
| ambiguous | 107 | 0 | 0 | 2 | 0 |
| subarray | 318316 | 0 | 0 | 0 | 0 |
| strings/basic | 87674 | 0 | 0 | 0 | 0 |
| strings/search | 876 | 0 | 0 | 0 | 0 |
| strings/util | 1147 | 0 | 0 | 0 | 0 |
| strings/io | 12764 | 0 | 0 | 0 | 0 |
| strings/types | 2302691 | 0 | 0 | 0 | 0 |
| unicode/utf8 | 19 | 0 | 0 | 0 | 0 |
| core | X | X | X | X | X |
| worlds | ☠️ | ☠️ | ☠️ | ☠️ | ☠️ |
| atomics | 3444 | 0 | 0 | 0 | 0 |
| keywordargs | 151 | 0 | 0 | 0 | 0 |
| numbers | 1578757 | 1 | 0 | 2 | 0 |
| subtype | 337674 | 0 | 0 | 19 | 0 |
| char | 1628 | 0 | 0 | 0 | 0 |
| triplequote | 29 | 0 | 0 | 0 | 0 |
| intrinsics | 301 | 0 | 0 | 0 | 0 |
| dict | 144420 | 0 | 0 | 0 | 0 |
| hashing | 12519 | 0 | 0 | 0 | 0 |
| iobuffer | 209 | 0 | 0 | 0 | 0 |
| staged | 59 | 5 | 0 | 0 | 0 |
| offsetarray | 484 | 3 | 0 | 3 | 0 |
| arrayops | 2031 | 0 | 0 | 2 | 0 |
| tuple | 625 | 0 | 0 | 0 | 0 |
| reduce | 8580 | 2 | 0 | 0 | 0 |
| reducedim | 865 | 0 | 0 | 0 | 0 |
| abstractarray | 55892 | 0 | 0 | 24795 | 0 |
| intfuncs | 227902 | 0 | 0 | 0 | 0 |
| simdloop | 240 | 0 | 0 | 0 | 0 |
| vecelement | 678 | 0 | 0 | 0 | 0 |
| rational | 98639 | 0 | 0 | 1 | 0 |
| bitarray | 914001 | 1 | 0 | 0 | 0 |
| copy | 533 | 0 | 0 | 0 | 0 |
| math | 148967 | 1 | 11 | 0 | 0 |
| fastmath | 946 | 0 | 0 | 0 | 0 |
| functional | 98 | 0 | 0 | 0 | 0 |
| iterators | 10164 | 0 | 0 | 0 | 0 |
| operators | 13039 | 0 | 1 | 0 | 0 |
| ordering | 37 | 0 | 0 | 0 | 0 |
| path | 1051 | 0 | 0 | 12 | 0 |
| ccall | X | X | X | X | X |
| parse | 16098 | 0 | 0 | 0 | 0 |
| loading | 168472 | 2 | 1 | 0 | 0 |
| gmp | 2357 | 0 | 0 | 0 | 0 |
| sorting | 16096 | 0 | 0 | 10 | 0 |
| spawn | 230 | 1 | 1 | 4 | 0 |
| backtrace | 13 | 9 | 16 | 1 | 0 |
| exceptions | 43 | 21 | 6 | 0 | 0 |
| file | X | X | X | X | X |
| read | 3870 | 0 | 0 | 0 | 0 |
| version | 2452 | 0 | 0 | 0 | 0 |
| namedtuple | 215 | 0 | 0 | 0 | 0 |
| mpfr | 1135 | 0 | 0 | 1 | 0 |
| broadcast | 509 | 2 | 0 | 0 | 0 |
| complex | 8432 | 0 | 0 | 5 | 0 |
| floatapprox | 49 | 0 | 0 | 0 | 0 |
| reflection | X | X | X | X | X |
| regex | 130 | 0 | 0 | 0 | 0 |
| float16 | 237 | 0 | 0 | 0 | 0 |
| combinatorics | 170 | 0 | 0 | 0 | 0 |
| sysinfo | 4 | 0 | 0 | 0 | 0 |
| env | 94 | 0 | 0 | 0 | 0 |
| rounding | 112720 | 0 | 0 | 0 | 0 |
| ranges | 12110652 | 2 | 0 | 327682 | 0 |
| mod2pi | 80 | 0 | 0 | 0 | 0 |
| euler | 12 | 0 | 0 | 0 | 0 |
| show | 128879 | 0 | 0 | 8 | 0 |
| client | 2 | 3 | 0 | 0 | 0 |
| errorshow | X | X | X | X | X |
| sets | 3594 | 0 | 0 | 1 | 0 |
| goto | 19 | 0 | 0 | 0 | 0 |
| llvmcall2 | 7 | 0 | 0 | 0 | 0 |
| ryu | 31215 | 0 | 0 | 0 | 0 |
| some | 72 | 0 | 0 | 0 | 0 |
| meta | 69 | 0 | 0 | 0 | 0 |
| stacktraces | X | X | X | X | X |
| docs | 237 | 1 | 0 | 0 | 0 |
| misc | X | X | X | X | X |
| threads | X | X | X | X | X |
| stress | 0 | 0 | 0 | 0 | 0 |
| binaryplatforms | 341 | 0 | 0 | 0 | 0 |
| atexit | 40 | 0 | 0 | 0 | 0 |
| enums | 99 | 0 | 0 | 0 | 0 |
| cmdlineargs | 255 | 0 | 0 | 3 | 0 |
| int | 524698 | 0 | 0 | 0 | 0 |
| interpreter | 3 | 0 | 0 | 0 | 0 |
| checked | 1239 | 0 | 0 | 0 | 0 |
| bitset | 195 | 0 | 0 | 0 | 0 |
| floatfuncs | 215 | 0 | 0 | 0 | 0 |
| precompile | X | X | X | X | X |
| boundscheck | X | X | X | X | X |
| error | 31 | 0 | 0 | 0 | 0 |
| cartesian | 343 | 0 | 0 | 3 | 0 |
| osutils | 57 | 0 | 0 | 0 | 0 |
| channels | 252 | 6 | 0 | 0 | 0 |
| iostream | 50 | 0 | 0 | 0 | 0 |
| secretbuffer | 27 | 0 | 0 | 0 | 0 |
| specificity | X | X | X | X | X |
| reinterpretarray | 232 | 0 | 0 | 0 | 0 |
| syntax | X | X | X | X | X |
| corelogging | 230 | 1 | 0 | 0 | 0 |
| missing | 565 | 0 | 0 | 1 | 0 |
| asyncmap | 304 | 0 | 0 | 0 | 0 |
| smallarrayshrink | 36 | 0 | 0 | 0 | 0 |
| opaque_closure | X | X | X | X | X |
| filesystem | 4 | 0 | 0 | 0 | 0 |
| download | X | X | X | X | X |
