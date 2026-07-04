\ toolchain-test.f - focused tests for PTX toolchain helper failures.

require lib/test.f
require lib/ptx/toolchain.f

package PTXTOOL-TEST

create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: ROOT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: PTXAS-FAILS ( -- )
   1 PTXTOOL:CHECK-PTXAS-RC ;

: TEMP-ROOT-WORKS ( -- )
   s" habu-ptx-toolchain-test" PTXTOOL:TEMP-ROOT ROOT!
   ROOT$ DIR? TTRUE
   ROOT$ REMOVE-TREE ;

: RUN ( -- )
   T-RESET
   PTXTOOL:PTXAS$ nip 0 > TTRUE
   0 PTXTOOL:CHECK-PTXAS-RC
   [: PTXAS-FAILS ;] E-PTX-PTXAS TTHROWSQ
   TEMP-ROOT-WORKS
   T-REPORT ;

RUN

end-package
