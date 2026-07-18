\ toolchain-test.f - checked tests for PTXTC artifact helpers.

require lib/test.f
require lib/adt/option.f
require lib/ptx/toolchain.f

package PTXTC-TEST

create PATH-A FS-PATH-CAP allot
create PATH-B FS-PATH-CAP allot
variable PATH-A-U
variable PATH-B-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PATH-A$ ( -- ptr u8 n )
   PATH-A PATH-A-U @ ;

: PATH-B$ ( -- ptr u8 n )
   PATH-B PATH-B-U @ ;

\ probe present (this host has ptxas): PTXAS$ returns the same non-empty path.
: ASSERT-PTXAS-PRESENT ( n -- )   \ n = TRY-PTXAS$ reported length
   PTXTC:PTXAS$ nip          \ ( probe-len ptxas-len )
   dup 0 > TTRUE             \ resolved path is non-empty
   = TTRUE ;                 \ and PTXAS$ agrees with the probe

\ Per host class: TRY-PTXAS$ is the fail-open probe. Present -> PTXAS$ resolves the
\ same path; absent (this CUDA-less Mac) -> PTXAS$ fails closed with E-PTXTC-PTXAS.
\ Both arms are real assertions; the present arm runs on a ptxas-equipped host.
: ASSERT-PTXAS ( -- )
   PTXTC:TRY-PTXAS$ MATCH option
     none OF [: PTXTC:PTXAS$ 2drop ;] E-PTXTC-PTXAS TTHROWSQ ENDOF
     some OF ASSERT-PTXAS-PRESENT ENDOF
   ;MATCH ;

: PREPARE-PATHS ( -- )
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ FILE? TFALSE
   PTXTC:CUBIN$ FILE? TFALSE
   ASSERT-PTXAS
   PTXTC:PTX$ s" ptx" WRITE-ALL
   PTXTC:PTX$ FILE? TTRUE
   PTXTC:CLEAN
   PTXTC:PTX$ FILE? TFALSE ;

: PREPARE-CLEANS-OLD ( -- )
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ PATH-A PATH-A-U COPY!
   PATH-A$ s" old" WRITE-ALL
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ PATH-B PATH-B-U COPY!
   PATH-A$ PATH-B$ T$<>
   PATH-A$ FILE? TFALSE
   PTXTC:CLEAN ;

\ ASSEMBLE with no arch configured must fail closed - there is no fallback target.
create ASM-O 64 allot   create ASM-E 64 allot

public

: RUN ( -- )
   T-RESET
   \ fail-closed before any TC-ARCH! runs in this process
   [: ASM-O 64 >LEN  ASM-E 64 >LEN  PTXTC:ASSEMBLE drop ;] E-PTXTC-ARCH TTHROWSQ
   PREPARE-PATHS
   PREPARE-CLEANS-OLD
   T-REPORT ;

;package

PTXTC-TEST:RUN

\ TC-ARCH! stores the assembler target and TC-ARCH$ reads it back (reopen the
\ owner package for the private reader).
package PTXTC
T-RESET
s" sm_121a" TC-ARCH!  TC-ARCH$ s" sm_121a" T$=
T-REPORT
;package
