\ device-artifacts-test.f - private grader artifact path tests.

require lib/test.f
require lib/adt/option.f
require maki/device-artifacts.f

package MAKI-GRADE-TEST

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
   MAKI-GRADE:PTXAS$ nip     \ ( probe-len ptxas-len )
   dup 0 > TTRUE             \ resolved path is non-empty
   = TTRUE ;                 \ and PTXAS$ agrees with the probe

\ Per host class: MAKI-GRADE:PTXAS$ delegates to the checked toolchain resolver, so
\ PTXTC:TRY-PTXAS$ is its fail-open probe. Present -> PTXAS$ resolves the same path;
\ absent (this CUDA-less Mac) -> PTXAS$ fails closed with E-PTXTC-PTXAS. Both arms are
\ real assertions; the present arm runs on a ptxas-equipped host.
: ASSERT-PTXAS ( -- )
   PTXTC:TRY-PTXAS$ MATCH option
     none OF [: MAKI-GRADE:PTXAS$ 2drop ;] E-PTXTC-PTXAS TTHROWSQ ENDOF
     some OF ASSERT-PTXAS-PRESENT ENDOF
   ;MATCH ;

: PREPARE-PATHS ( -- )
   s" habu-grade-artifacts" MAKI-GRADE:PREPARE
   MAKI-GRADE:DRIVER$ FILE? TFALSE
   MAKI-GRADE:PTX$ FILE? TFALSE
   MAKI-GRADE:CUBIN$ FILE? TFALSE
   ASSERT-PTXAS
   MAKI-GRADE:DRIVER$ s" x" WRITE-ALL
   MAKI-GRADE:DRIVER$ FILE? TTRUE
   MAKI-GRADE:CLEAN
   MAKI-GRADE:DRIVER$ FILE? TFALSE ;

: PREPARE-CLEANS-OLD ( -- )
   s" habu-grade-artifacts" MAKI-GRADE:PREPARE
   MAKI-GRADE:DRIVER$ PATH-A PATH-A-U COPY!
   PATH-A$ s" old" WRITE-ALL
   s" habu-grade-artifacts" MAKI-GRADE:PREPARE
   MAKI-GRADE:DRIVER$ PATH-B PATH-B-U COPY!
   PATH-A$ PATH-B$ T$<>
   PATH-A$ FILE? TFALSE
   MAKI-GRADE:CLEAN ;

public

: RUN ( -- )
   T-RESET
   PREPARE-PATHS
   PREPARE-CLEANS-OLD
   T-REPORT ;

;package

MAKI-GRADE-TEST:RUN
