\ fs-stream-test.f - checked no-follow descriptor streaming tests.
\ Run: bin/hb --load lib/fs-stream-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require test/checker-assert.f

package FS-STREAM-TEST
public

1 constant EX-FAIL
-60 constant OPEN-RC
-61 constant STAT-RC
-62 constant READ-RC
-63 constant CLOSE-RC
4 constant CHUNK-CAP
32 constant OUT-CAP

variable ROOT-U
variable REG-U
variable EMPTY-U
variable LINK-U
variable BROKEN-U
variable MISSING-U
variable RACE-U
variable OUT-U
variable CHUNK-N
variable CLOSE-N
variable OPEN-FD

create ROOT-BUF FS-PATH-CAP allot
create REG-BUF FS-PATH-CAP allot
create EMPTY-BUF FS-PATH-CAP allot
create LINK-BUF FS-PATH-CAP allot
create BROKEN-BUF FS-PATH-CAP allot
create MISSING-BUF FS-PATH-CAP allot
create RACE-BUF FS-PATH-CAP allot
create CHUNK-BUF CHUNK-CAP allot
create OUT OUT-CAP allot
create STAT FS:STAT-BYTES allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: REG$ ( -- ptr u8 n )
   REG-BUF REG-U @ ;

: EMPTY$ ( -- ptr u8 n )
   EMPTY-BUF EMPTY-U @ ;

: LINK$ ( -- ptr u8 n )
   LINK-BUF LINK-U @ ;

: BROKEN$ ( -- ptr u8 n )
   BROKEN-BUF BROKEN-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: RACE$ ( -- ptr u8 n )
   RACE-BUF RACE-U @ ;

: PATHS! ( -- )
   ROOT$ s" regular.bin" REG-BUF REG-U PATH!
   ROOT$ s" empty.bin" EMPTY-BUF EMPTY-U PATH!
   ROOT$ s" regular-link" LINK-BUF LINK-U PATH!
   ROOT$ s" broken-link" BROKEN-BUF BROKEN-U PATH!
   ROOT$ s" missing.bin" MISSING-BUF MISSING-U PATH!
   ROOT$ s" race.bin" RACE-BUF RACE-U PATH! ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-fs-stream" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   PATHS!
   REG$ s" abcdefghijklmnopq" WRITE-ALL
   EMPTY$ s" " WRITE-ALL
   RACE$ s" old" WRITE-ALL
   REG$ LINK$ MAKE-SYMLINK
   MISSING$ BROKEN$ MAKE-SYMLINK ;

: RESET-OUT ( -- )
   0 OUT-U !
   0 CHUNK-N ! ;

\ typed-local-lint: allow-bare-local - src preserves the callback ptr u8 role.
: CHUNK ( ptr u8 n -- ) {: src u:n :}
   OUT-U @ u + OUT-CAP > if E-FS-CAPACITY throw then
   src OUT OUT-U @ + u BYTE-COPY
   OUT-U @ u + OUT-U !
   CHUNK-N @ 1 + CHUNK-N ! ;

: CALLBACK-FAIL ( ptr u8 n -- )
   2drop EX-FAIL throw ;

: NOOP ( ptr u8 n -- )
   2drop ;

: EXPECT-OK ( FS:stream-outcome -- )
   MATCH FS:stream-outcome
      ok OF FS-TRUE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH TTRUE ;

: CONSUME ( FS:stream-outcome -- )
   MATCH FS:stream-outcome
      ok OF ENDOF
      failed OF drop ENDOF
      close-failed OF drop ENDOF
      failed-close OF 2drop ENDOF
   ;MATCH ;

: EXPECT-FAILED ( FS:stream-outcome n -- ) {: want:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF want = ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH TTRUE ;

: EXPECT-CLOSE ( FS:stream-outcome n -- ) {: want:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF RC>N want = ENDOF
      failed-close OF 2drop FS-FALSE ENDOF
   ;MATCH TTRUE ;

: EXPECT-BOTH ( FS:stream-outcome n n -- )
   {: primary:n close:n :}
   MATCH FS:stream-outcome
      ok OF FS-FALSE ENDOF
      failed OF drop FS-FALSE ENDOF
      close-failed OF drop FS-FALSE ENDOF
      failed-close OF
         {: got:n crc:rc :}
         got primary = crc RC>N close = and
      ENDOF
   ;MATCH TTRUE ;

: SAME-FD ( fd -- )
   FD>N OPEN-FD @ T= ;

: SAME-STAT ( ptr u8 -- )
   STAT = TTRUE ;

: CLEAN ( -- )
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE ;

;package

FS-STREAM-TEST:PREPARE
T-RESET

\ Test-owned private reopen. The production package publishes no injection seam.
package FS
private

: TEST-OPEN-FAIL ( ptr u8 -- n )
   drop FS-STREAM-TEST:OPEN-RC ;

: TEST-STAT-FAIL ( fd ptr u8 -- rc )
   2drop FS-STREAM-TEST:STAT-RC >RC ;

: TEST-READ-FAIL ( fd ptr u8 n -- n )
   2drop drop FS-STREAM-TEST:READ-RC ;

: TEST-READ-OVERRUN ( fd ptr u8 n -- n )
   {: fd:fd buf:ptr cap:n :}
   cap 1 + ;

: TEST-REAL-CLOSE ( fd -- )
   CLOSE-FD RC>N 0 <> if E-FS-IO throw then ;

: TEST-CLOSE-COUNT ( fd -- rc ) {: fd:fd :}
   FS-STREAM-TEST:CLOSE-N @ 1 + FS-STREAM-TEST:CLOSE-N !
   fd CLOSE-FD ;

: TEST-CLOSE-FAIL ( fd -- rc ) {: fd:fd :}
   fd TEST-REAL-CLOSE
   FS-STREAM-TEST:CLOSE-RC >RC ;

: TEST-CLOSE-FAIL-COUNT ( fd -- rc ) {: fd:fd :}
   FS-STREAM-TEST:CLOSE-N @ 1 + FS-STREAM-TEST:CLOSE-N !
   fd TEST-CLOSE-FAIL ;

: TEST-TRACK-OPEN ( ptr u8 -- n )
   OPEN-RAW dup FS-STREAM-TEST:OPEN-FD ! ;

: TEST-TRACK-STAT ( fd ptr u8 -- rc ) {: fd:fd stat:ptr :}
   fd FS-STREAM-TEST:SAME-FD
   stat FS-STREAM-TEST:SAME-STAT
   fd stat FSTAT-FD ;

: TEST-TRACK-READ ( fd ptr u8 n -- n )
   {: fd:fd buf:ptr cap:n :}
   fd FS-STREAM-TEST:SAME-FD
   fd buf cap READ-FD ;

: TEST-TRACK-CLOSE ( fd -- rc ) {: fd:fd :}
   fd FS-STREAM-TEST:SAME-FD
   fd CLOSE-FD ;

: TEST-RACE-OPEN ( ptr u8 -- n )
   drop
   FS-STREAM-TEST:RACE$ REMOVE-FILE
   FS-STREAM-TEST:REG$ FS-STREAM-TEST:RACE$ MAKE-SYMLINK
   FS-STREAM-TEST:RACE$ FS-PATHZ OPEN-RAW ;

: TEST-ARGS ( -- ptr u8 n ptr u8 n ptr u8 n )
   FS-STREAM-TEST:REG$
   FS-STREAM-TEST:CHUNK-BUF FS-STREAM-TEST:CHUNK-CAP
   FS-STREAM-TEST:STAT FS:STAT-BYTES ;

: TEST-STAT ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: TEST-STAT-FAIL ;]
   [: READ-FD ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

: TEST-STAT-CLOSE ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: TEST-STAT-FAIL ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-READ ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-FAIL ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

: TEST-READ-CLOSE ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-FAIL ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-OVERRUN ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-OVERRUN ;]
   [: CLOSE-FD ;]
   STREAM-WITH ;

: TEST-OVERRUN-CLOSE ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: TEST-READ-OVERRUN ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-CALLBACK ( -- stream-outcome )
   0 FS-STREAM-TEST:CLOSE-N !
   TEST-ARGS
   [: FS-STREAM-TEST:CALLBACK-FAIL ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-COUNT ;]
   STREAM-WITH ;

: TEST-CALLBACK-CLOSE ( -- stream-outcome )
   0 FS-STREAM-TEST:CLOSE-N !
   TEST-ARGS
   [: FS-STREAM-TEST:CALLBACK-FAIL ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL-COUNT ;]
   STREAM-WITH ;

: TEST-CLOSE ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: OPEN-RAW ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-FAIL ;]
   STREAM-WITH ;

: TEST-TRACKED ( -- stream-outcome )
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: TEST-TRACK-OPEN ;]
   [: TEST-TRACK-STAT ;]
   [: TEST-TRACK-READ ;]
   [: TEST-TRACK-CLOSE ;]
   STREAM-WITH ;

: TEST-OPEN-ERROR ( -- )
   0 FS-STREAM-TEST:CLOSE-N !
   TEST-ARGS
   [: FS-STREAM-TEST:NOOP ;]
   [: TEST-OPEN-FAIL ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: TEST-CLOSE-COUNT ;]
   STREAM-WITH FS-STREAM-TEST:CONSUME ;

: TEST-RACE ( -- )
   FS-STREAM-TEST:RACE$
   FS-STREAM-TEST:CHUNK-BUF FS-STREAM-TEST:CHUNK-CAP
   FS-STREAM-TEST:STAT FS:STAT-BYTES
   [: FS-STREAM-TEST:NOOP ;]
   [: TEST-RACE-OPEN ;]
   [: FSTAT-FD ;]
   [: READ-FD ;]
   [: CLOSE-FD ;]
   STREAM-WITH FS-STREAM-TEST:CONSUME ;

: TEST-DOUBLE-CLOSE ( -- )
   FS-STREAM-TEST:REG$ FS-PATHZ OPEN-RAW OPEN>FD {: fd:fd :}
   fd CLOSE-FD RC>N 0 =
   fd CLOSE-FD RC>N 0 <> and
   TTRUE ;

: RUN-SEAMS ( -- )
   [: TEST-OPEN-ERROR ;] E-FS-OPEN TTHROWSQ
   FS-STREAM-TEST:CLOSE-N @ 0 T=
   TEST-STAT E-FS-STAT FS-STREAM-TEST:EXPECT-FAILED
   TEST-READ E-FS-IO FS-STREAM-TEST:EXPECT-FAILED
   TEST-OVERRUN E-FS-IO FS-STREAM-TEST:EXPECT-FAILED
   TEST-CALLBACK FS-STREAM-TEST:EX-FAIL FS-STREAM-TEST:EXPECT-FAILED
   FS-STREAM-TEST:CLOSE-N @ 1 T=
   TEST-CLOSE FS-STREAM-TEST:CLOSE-RC FS-STREAM-TEST:EXPECT-CLOSE
   TEST-STAT-CLOSE E-FS-STAT FS-STREAM-TEST:CLOSE-RC
      FS-STREAM-TEST:EXPECT-BOTH
   TEST-READ-CLOSE E-FS-IO FS-STREAM-TEST:CLOSE-RC
      FS-STREAM-TEST:EXPECT-BOTH
   TEST-OVERRUN-CLOSE E-FS-IO FS-STREAM-TEST:CLOSE-RC
      FS-STREAM-TEST:EXPECT-BOTH
   TEST-CALLBACK-CLOSE FS-STREAM-TEST:EX-FAIL FS-STREAM-TEST:CLOSE-RC
      FS-STREAM-TEST:EXPECT-BOTH
   FS-STREAM-TEST:CLOSE-N @ 1 T=
   TEST-TRACKED FS-STREAM-TEST:EXPECT-OK
   TEST-DOUBLE-CLOSE
   [: TEST-RACE ;] E-FS-OPEN TTHROWSQ
   FS-STREAM-TEST:RACE$ SYMLINK? TTRUE ;

RUN-SEAMS

;package

package FS-STREAM-TEST
private

: STREAM-MISSING ( -- )
   MISSING$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: NOOP ;] FS:STREAM-REGULAR CONSUME ;

: STREAM-LINK ( -- )
   LINK$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: NOOP ;] FS:STREAM-REGULAR CONSUME ;

: STREAM-BROKEN ( -- )
   BROKEN$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: NOOP ;] FS:STREAM-REGULAR CONSUME ;

: STREAM-DIR ( -- FS:stream-outcome )
   ROOT$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: NOOP ;] FS:STREAM-REGULAR ;

: STREAM-ZERO-CHUNK ( -- )
   REG$ CHUNK-BUF 0 STAT FS:STAT-BYTES
   [: NOOP ;] FS:STREAM-REGULAR CONSUME ;

: STREAM-SHORT-STAT ( -- )
   REG$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES 1 -
   [: NOOP ;] FS:STREAM-REGULAR CONSUME ;

: TEST-STREAM ( -- )
   RESET-OUT
   REG$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: CHUNK ;] FS:STREAM-REGULAR EXPECT-OK
   OUT-U @ 17 T=
   CHUNK-N @ 1 > TTRUE
   OUT OUT-U @ s" abcdefghijklmnopq" STR= TTRUE
   RESET-OUT
   EMPTY$ CHUNK-BUF CHUNK-CAP STAT FS:STAT-BYTES
   [: CHUNK ;] FS:STREAM-REGULAR EXPECT-OK
   OUT-U @ 0 T=
   CHUNK-N @ 0 T= ;

: TEST-ERRORS ( -- )
   [: STREAM-LINK ;] E-FS-OPEN TTHROWSQ
   [: STREAM-BROKEN ;] E-FS-OPEN TTHROWSQ
   [: STREAM-MISSING ;] E-FS-OPEN TTHROWSQ
   STREAM-DIR E-FS-STAT EXPECT-FAILED
   [: STREAM-ZERO-CHUNK ;] E-FS-CAPACITY TTHROWSQ
   [: STREAM-SHORT-STAT ;] E-FS-CAPACITY TTHROWSQ ;

: TEST-TYPES ( -- )
   s" FSP ( ptr u8 n ptr u8 n ptr u8 n [ ptr u8 n -- ] -- FS:stream-outcome ) FS:STREAM-REGULAR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" FSN ( ptr u8 n ptr u8 n ptr u8 n [ ptr u8 -- ] -- FS:stream-outcome ) FS:STREAM-REGULAR"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" FOK ( -- FS:stream-outcome ) FS-STREAM--OUTCOME:OK"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" FFAILED ( n -- FS:stream-outcome ) FS-STREAM--OUTCOME:FAILED"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" FBAD ( fd -- FS:stream-outcome ) FS-STREAM--OUTCOME:FAILED"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" FPRIVATE ( -- FS:stream-outcome ) construct stream-outcome ok"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: TEST-CTORS ( -- )
   FS-STREAM--OUTCOME:OK EXPECT-OK
   E-FS-IO FS-STREAM--OUTCOME:FAILED E-FS-IO EXPECT-FAILED
   CLOSE-RC >RC FS-STREAM--OUTCOME:CLOSE-FAILED CLOSE-RC EXPECT-CLOSE
   E-FS-IO CLOSE-RC >RC FS-STREAM--OUTCOME:FAILED-CLOSE
      E-FS-IO CLOSE-RC EXPECT-BOTH ;

: MAIN ( -- )
   TEST-STREAM
   TEST-ERRORS
   TEST-TYPES
   TEST-CTORS
   CLEAN
   T-REPORT
   s" fs-stream-test: ok" type cr ;

MAIN

;package
