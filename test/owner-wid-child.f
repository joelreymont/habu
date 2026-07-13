\ owner-wid-child.f - build and run the cold owner proof in a child process.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-HARNESS

require test/owner-wid-image.f
require lib/test.f

package OWNER-WID-CHILD

$4000 constant CAP
120000 constant TIMEOUT-MS
0 constant KIND-EXITED
1 constant KIND-SIGNALED
2 constant KIND-TIMEOUT
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RUN-KIND
variable RUN-CODE

: RUN-KIND! ( outcome -- )
   MATCH outcome
     exited OF RUN-CODE ! KIND-EXITED RUN-KIND ! ENDOF
     signaled OF RUN-CODE ! KIND-SIGNALED RUN-KIND ! ENDOF
     timeout OF 0 RUN-CODE ! KIND-TIMEOUT RUN-KIND ! ENDOF
   ;MATCH ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: KIND$ ( -- ptr u8 n )
   RUN-KIND @ KIND-TIMEOUT = if s" timeout" exit then
   s" signal" ;

: DIAG-CMD ( ptr u8 n -- ) {: file:ptr fileu:n :}
   s" owner-wid-child: abnormal run-file: " type
   OWNER-WID-IMAGE:HB$ type
   s"  --load " type file fileu type cr
   s" owner-wid-child: outcome " type KIND$ type
   s"  code " type RUN-CODE @ .
   s" timeout-ms " type TIMEOUT-MS . cr ;

: DIAG-STREAM ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n a:ptr u:n :}
   s" owner-wid-child: " type label labelu type
   s"  bytes " type u .
   s" cap " type CAP . cr
   a u type cr ;

: RUN-DIAG ( ptr u8 n -- ) {: file:ptr fileu:n :}
   file fileu DIAG-CMD
   s" stdout" OUT$ DIAG-STREAM
   s" stderr" ERR$ DIAG-STREAM ;

\ Timeboxed child run: the capture poll is bounded by TIMEOUT-MS; a timeout or
\ signal death reports a named diagnostic and returns -1 so the caller's exact
\ rc assertion fails instead of an unattributed E-PROC-TIMEOUT throw.
: RUN-FILE ( ptr u8 n -- n ) {: file:ptr fileu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   file fileu >LEN PROC-ARGV+
   OWNER-WID-IMAGE:HB$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   RUN-KIND!
   LEN>N ERR-U !
   LEN>N OUT-U !
   RUN-KIND @ KIND-EXITED = if RUN-CODE @ exit then
   file fileu RUN-DIAG
   -1 ;

: BODY ( -- )
   OWNER-WID-IMAGE:BUILD
   BUILD-EXT:ASSERT-EMPTY
   s" test/owner-wid-state.f" RUN-FILE 0 T=
   OUT$ s" owner-wid-state-test: ok" CONTAINS? TTRUE
   s" test/owner-wid-call.f" RUN-FILE 70 T=
   ERR$ s" owner-wid-add" CONTAINS? TTRUE
   s" test/owner-wid-build-forge.f" RUN-FILE 70 T=
   ERR$ s" SET" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" owner-wid-child-test: ok" type cr ;

;package

OWNER-WID-CHILD:RUN
