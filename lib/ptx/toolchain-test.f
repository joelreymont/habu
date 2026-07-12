\ toolchain-test.f - checked tests for PTXTC artifact helpers.

require lib/test.f
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

: PREPARE-PATHS ( -- )
   s" habu-ptx-toolchain" PTXTC:PREPARE
   PTXTC:PTX$ FILE? TFALSE
   PTXTC:CUBIN$ FILE? TFALSE
   PTXTC:PTXAS$ nip 0 > TTRUE
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

public

: RUN ( -- )
   T-RESET
   PREPARE-PATHS
   PREPARE-CLEANS-OLD
   T-REPORT ;

;package

PTXTC-TEST:RUN
