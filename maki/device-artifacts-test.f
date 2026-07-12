\ device-artifacts-test.f - private grader artifact path tests.

require lib/test.f
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

: PREPARE-PATHS ( -- )
   s" habu-grade-artifacts" MAKI-GRADE:PREPARE
   MAKI-GRADE:DRIVER$ FILE? TFALSE
   MAKI-GRADE:PTX$ FILE? TFALSE
   MAKI-GRADE:CUBIN$ FILE? TFALSE
   MAKI-GRADE:PTXAS$ nip 0 > TTRUE
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
