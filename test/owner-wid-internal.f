\ owner-wid-internal.f - build and run the isolated cold owner proof.

require test/owner-wid-image.f
require lib/test.f

package OWNER-WID-INTERNAL

$4000 constant CAP
120000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: RUN-FILE ( ptr u8 n -- n ) {: file:ptr fileu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   file fileu >LEN PROC-ARGV+
   OWNER-WID-IMAGE:HB$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu:len erru:len code:rc :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   code RC>N ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: BODY ( -- )
   OWNER-WID-IMAGE:BUILD
   BUILD-EXT:ASSERT-EMPTY
   BF-TMP-U @ 0 T=
   BF-ENGINE-U @ 0 T=
   BF-PIN-N @ 0 T=
   BF-PIN-ON @ 0 T=
   BF-REC-STAGE? @ 0 T=
   BF-REC-STDIN? @ 0 T=
   s" test/owner-wid-state.f" RUN-FILE 0 T=
   OUT$ s" owner-wid-state-test: ok" CONTAINS? TTRUE
   s" test/owner-wid-call.f" RUN-FILE 70 T=
   ERR$ s" owner-wid-add" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" owner-wid-internal-test: ok" type cr ;

;package

OWNER-WID-INTERNAL:RUN
