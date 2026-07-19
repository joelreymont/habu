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

\ TC-ARCH!/TC-ARCH$ plus version-aware resolution and the stale-toolchain
\ diagnostic (dot habu-pin-blackwell-grade-8ec5ee0a): reopen the owner package so
\ the private version readers and the store probe are in scope.
package PTXTC
T-RESET

\ PARSE-VER discriminates the two assemblers that motivated the pin (13.3 = the
\ Blackwell-grade scheduler, 13.0 = the 40-NOP one) and rejects malformed text.
: T-VER-PARSE ( -- )
   s" Cuda compilation tools, release 13.3, V13.3.33" PARSE-VER PTXAS-PINNED-VER T=
   s" Cuda compilation tools, release 13.0, V13.0.88" PARSE-VER 13000 T=
   s" no release line here"  PARSE-VER -1 T=
   s" release 13"            PARSE-VER -1 T=
   s" release 13.x"          PARSE-VER -1 T= ;

\ SM121-TARGET? keys off the configured arch (the profile the pin protects).
: T-SM121 ( -- )
   s" sm_121a" TC-ARCH!  SM121-TARGET? TTRUE
   s" sm_90a"  TC-ARCH!  SM121-TARGET? TFALSE ;

\ The diagnostic fires only for an sm_121 target with a KNOWN pre-13.3 ptxas;
\ silent for the pin, for newer, for an unreadable version, and for non-sm_121.
: T-STALE ( -- )
   s" sm_121a" TC-ARCH!
   13000 STALE-SM121? TTRUE
   PTXAS-PINNED-VER STALE-SM121? TFALSE
   -1 STALE-SM121? TFALSE
   s" sm_90a" TC-ARCH!
   13000 STALE-SM121? TFALSE ;

\ Habu's own tool store is in the probe list at $HOME/.habu/toolchain/ptxas-13.3.33.
: T-STORE-SHAPE ( -- )
   s" HOME" GETENV nip 0 > if
      STORE-PTXAS$ s" /.habu/toolchain/ptxas-13.3.33" CONTAINS? TTRUE
   then ;

\ Present arm (provisioned host, PTXAS unset): the pinned store wins over system
\ CUDA and its 13.3 version reads back through the real spawn+parse path.
: T-STORE-RESOLVES ( -- )
   s" PTXAS" GETENV nip 0= if
      STORE-PTXAS$ FILE? if
         PTXAS$ STORE-PTXAS$ T$=
         PTXAS$ PTXAS-VERSION PTXAS-PINNED-VER T=
      then
   then ;

T-VER-PARSE  T-SM121  T-STALE  T-STORE-SHAPE  T-STORE-RESOLVES
s" sm_121a" TC-ARCH!  TC-ARCH$ s" sm_121a" T$=
T-REPORT
;package
